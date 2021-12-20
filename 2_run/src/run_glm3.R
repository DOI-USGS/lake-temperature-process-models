#' @title Munge meteo and add burn-in and burn-out
#' @description function to read in the raw meteorological data
#' form the file specified in the model_config, filter it based
#' on the specified time_period, and add burn-in and burn-out
#' periods by mirroring the data. From Jordan's GLM projection code: 
#' https://github.com/jread-usgs/lake-temperature-process-models/blob/master/3_run/src/run_glm_utils.R#L62-L76
#' @param model_config a lake-gcm-time_period crosswalk table with the 
#'  meteo file name and hash for each model run
#'  @param begin the begin date for the model time_period
#'  @param end the end date for the model_time_period
#'  @param burn_in length of burn-in period, in days
#'  @param burn_out length of burn-out period, in days
#'  @return a munged dataframe of meteorological data that includes
#'  burn-in and burn-out time
munge_meteo_w_burn_in_out <- function(model_config, begin, end, burn_in = 300, burn_out = 190){
  meteo_data <- arrow::read_feather(model_config$meteo_fl) %>%
    mutate(time = as.Date(time)) %>% filter(time >= as.Date(begin) & time <= as.Date(end)) %>%
    mutate(Rain = case_when(Snow > 0 ~ 0, TRUE ~ Rain))
  # add the burn-in/burn-out if the requested burn-in/burn-out period is 
  # shorter than the length of the raw meteo data
  ndays_meteo <- nrow(meteo_data) #data is formatted as one row per day
  if (burn_in <= ndays_meteo) {
    # create a burn-in. We're going to mirror the data of the first year:
    burn_in_data <- meteo_data[2:burn_in, ] %>% arrange(desc(time)) %>%
      mutate(time = seq(from = meteo_data[1,]$time-burn_in+1, by = 'day', length.out = length(2:burn_in)))
    # bind with real data, prior to the begin date. We're not duplicating dates, hence the "2"
    meteo_data <- bind_rows(burn_in_data, meteo_data)
  } else {
    message(paste(sprintf('The requested burn-in length (%s days) exceeds', burn_in),
                  sprintf('the length of the raw meteorological data (%s days).', ndays_meteo),
                   'No burn-in will be added.',
                  sep = "\n"))
  }
  if (burn_out <= ndays_meteo) {
    # create a burn-out. Mirror the final data:
    burn_out_data <- tail(meteo_data, burn_out) %>% head(-1L) %>% arrange(desc(time)) %>%
      mutate(time = seq(from = tail(meteo_data,1)$time+1, by = 'day', length.out = length(2:burn_out)))
    # bind with real data, at the end of the sequence
    meteo_data <- bind_rows(meteo_data, burn_out_data)
  } else {
    message(paste(sprintf('The requested burn-out length (%s days) exceeds', burn_out),
                  sprintf('the length of the raw meteorological data (%s days).', ndays_meteo),
                  'No burn-out will be added.',
                  sep = "\n"))
  }
  return(meteo_data)
}

#' @title function to extract and write glm output following a model run
#' @description Extract temperature and ice data from the model
#' run and save to the export feather file. 
#' From Jordan's GLM projection code: 
#' https://github.com/jread-usgs/lake-temperature-process-models/blob/master/3_run/src/run_glm_utils.R#L89-L101
#' @param sim_lake_dir the directory for the model simulation
#' @param nml_obj the complete nml object for the model run
#' @param export_fl the filepath for the exported feather file
extract_glm_output <- function(nc_filepath, nml_obj, export_fl) {
  lake_depth <- glmtools::get_nml_value(nml_obj, arg_name = 'lake_depth')
  export_depths <- seq(0, lake_depth, by = 0.5)
  temp_data <- glmtools::get_temp(nc_filepath, reference = 'surface', z_out = export_depths) %>%
    mutate(date = as.Date(lubridate::floor_date(DateTime, 'days'))) %>% select(-DateTime)
  glmtools::get_var(nc_filepath, var_name = 'hice') %>%
    dplyr::mutate(ice = hice > 0, date = as.Date(lubridate::ceiling_date(DateTime, ' days'))) %>% dplyr::select(-hice, -DateTime) %>%
    dplyr::left_join(temp_data, ., by = 'date') %>%
    select(time = date, everything()) %>%
    arrow::write_feather(export_fl)
}

#' @title run glm3 simulation
#' @description For each lake - gcm - time period combo, write the nml
#'  file, the meteo data, and run the glm3 model. If the model run is
#'  successful, the results are extracted and saved to '2_run/tmp' using
#'  `extract_glm_ouput()` and then the simulation directory is deleted 
#'  @param sim_dir base directory for simulations
#'  @param nml_objs list of nml objects (one per lake id), named by lake id
#'  @param model_config a lake-gcm-time_period crosswalk table with the 
#'  meteo file name and hash for each model run
#'  @param export_fl_template the template for constructing the filepath 
#'  for the export feather file that will be saved in `extract_glm_ouput()`
#'  @return a tibble which includes the run_date, lake_id, gcm, time_period, 
#'  the name of the export feather file, its hash (NA if the model run failed), 
#'  the duration of the model run, whether or not the model run succeeded, 
#'  and the code returned by the call to GLM3r::run_glm(). 
run_glm3_model <- function(sim_dir, nml_objs, model_config, burn_in, burn_out, export_fl_template) {
  # pull lake_id from model_config
  lake_id <- model_config$site_id
  time_period <- model_config$time_period
  gcm <- model_config$gcm
  
  # prepare to write inputs and results locally for quick I/O
  sim_lake_dir <- file.path(sim_dir, sprintf('%s_%s_%s', lake_id, gcm, time_period))
  dir.create(sim_lake_dir, recursive=TRUE, showWarnings=FALSE)
  # delete sim_lake_dir after model has run and we've extracted the results
  on.exit(unlink(sim_lake_dir, recursive = TRUE))

  # Define time period begin and end dates
  times <- strsplit(time_period,'_')[[1]]
  time_period_begin <- sprintf('%s-01-01', times[1])
  time_period_end <- sprintf('%s-12-31', times[2])
  
  # Read in meteo data, add burn in and burn out and save to sim_lake_dir
  meteo_data <- munge_meteo_w_burn_in_out(model_config, time_period_begin, time_period_end, 
                                          burn_in = burn_in, burn_out = burn_out)
  sim_meteo_filename <- 'meteo_fl.csv'
  readr::write_csv(meteo_data, file.path(sim_lake_dir, sim_meteo_filename))
  
  # Define simulation start and stop dates using munged meteo data
  sim_start <- format(meteo_data[1,]$time, "%Y-%m-%d")
  sim_stop <- format(tail(meteo_data, 1L)$time, "%Y-%m-%d")
  
  # write nml file, specifying meteo file and start and stop dates:
  nml_obj <- nml_objs[[lake_id]]
  nml_obj <- set_nml(nml_obj, arg_list = list(nsave = 24, 
                                              meteo_fl = sim_meteo_filename,
                                              sim_name = sprintf('%s_%s_%s', lake_id, gcm, time_period),
                                              start = sim_start,
                                              stop = sim_stop))
  glmtools::write_nml(nml_obj, file.path(sim_lake_dir, 'glm3.nml'))
  
  # In prep for checking model if model successfully ran and extracting
  #  output, define path to output netCDF file
  out_dir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  out_fn <- paste0(glmtools::get_nml_value(nml_obj, 'out_fn'), '.nc')
  nc_filepath <- file.path(sim_lake_dir, out_dir, out_fn)
  
  # for each model run, try running the model up to 5 times
  # if model run succeeds (returned code = 0 AND final output
  # date coincides with the end of the simulation period), 
  # extract output and save to feather export file, and
  # return tibble with export file name, its hash and run
  # diagnostics. If model run fails after 5 attempts, return
  # tibble with model diagnostics and indicate that it failed.
  tryCatch(
    {
      retry::retry(
        {glm_time <- system.time({glm_code <- GLM3r::run_glm(sim_lake_dir, verbose = FALSE)})[['elapsed']]
        output_dates <- glmtools::get_temp(nc_filepath) %>%
          mutate(date = format(as.Date(lubridate::floor_date(DateTime, 'days')),"%Y-%m-%d")) %>%
          pull(date)
        max_output_date <- max(output_dates)
        },
        until=function(val, cnd) glm_code == 0 & max_output_date==sim_stop,
        max_tries = 5)

      # make sure glm did succeed  
      if(glm_code != 0 | max_output_date!=sim_stop) stop()
      
      # extract output
      export_fl <- sprintf(export_fl_template, lake_id, gcm, time_period)
      extract_glm_output(nc_filepath, nml_obj, export_fl)
      
      # Build export tibble with export file, its hash, and glm run information
      export_tibble <- tibble(
        lake_id = lake_id,
        gcm = gcm,
        time_period = time_period,
        export_fl = export_fl,
        export_fl_hash = tools::md5sum(export_fl),
        glm_run_date = Sys.time(),
        glm_version = GLM3r::glm_version(as_char = TRUE), #Needs version 3.1.18 of GLM3r
        glm_time_s = glm_time,
        param_sim_start = sim_start,
        param_sim_stop = sim_stop,
        max_output_date = max_output_date,
        glm_code = glm_code,
        glm_success = TRUE)
      return(export_tibble)
    },
    error = function(e) {
      # Build export tibble with glm run information
      # set export_fl and export_fl_hash to NA
      # to make sure previously exported files aren't tracked
      export_tibble <- tibble(
        lake_id = lake_id,
        gcm = gcm,
        time_period = time_period,
        export_fl = NA,
        export_fl_hash = NA,
        glm_run_date = Sys.time(),
        glm_version = GLM3r::glm_version(as_char = TRUE), #Needs version 3.1.18 of GLM3r
        glm_time_s = glm_time,
        param_sim_start = sim_start,
        param_sim_stop = sim_stop,
        max_output_date = max_output_date,
        glm_code = glm_code,
        glm_success = FALSE)
      return(export_tibble)
    }
  )
}
