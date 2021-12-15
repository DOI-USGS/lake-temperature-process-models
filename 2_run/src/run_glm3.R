#' @title Munge meteo and add burn-in and burn-out
#' @description function to read in the raw meteorological data
#' form the file specified in the meteo_xwalk, filter it based
#' on the specified time_period, and add burn-in and burn-out
#' periods by mirroring the data. From Jordan's GLM projection code: 
#' https://github.com/jread-usgs/lake-temperature-process-models/blob/master/3_run/src/run_glm_utils.R#L62-L76
#' @param meteo_xwalk a lake-gcm-time_period crosswalk table with the 
#'  meteo file name and hash for each model run
#'  @param begin the begin date for the model time_period
#'  @param end the end date for the model_time_period
#'  @param burn_in length of burn-in period, in days
#'  @param burn_out length of burn-out period, in days
#'  @return a munged dataframe of meteorological data that includes
#'  burn-in and burn-out time
munge_meteo_w_burn_in_out <- function(meteo_xwalk, begin, end, burn_in, burn_out){
  meteo_data <- arrow::read_feather(meteo_xwalk$meteo_fl) %>%
    mutate(time = as.Date(time)) %>% filter(time >= as.Date(begin) & time <= as.Date(end)) %>%
    mutate(Rain = case_when(Snow > 0 ~ 0, TRUE ~ Rain))
  # create a burn-in. We're going to mirror the data of the first year:
  burn_in_data <- meteo_data[2:burn_in, ] %>% arrange(desc(time)) %>%
    mutate(time = seq(from = meteo_data[1,]$time-burn_in+1, by = 'day', length.out = length(2:burn_in)))
  # bind with real data, prior to the begin date. We're not duplicating dates, hence the "2"
  meteo_data <- bind_rows(burn_in_data, meteo_data)
  # create a burn-out. Mirror the final data:
  burn_out_data <- tail(meteo_data, burn_out) %>% head(-1L) %>% arrange(desc(time)) %>%
    mutate(time = seq(from = tail(meteo_data,1)$time+1, by = 'day', length.out = length(2:burn_out)))
  # bind with real data, at the end of the sequence
  meteo_data <- bind_rows(meteo_data, burn_out_data)
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
extract_glm_output <- function(sim_lake_dir, nml_obj, export_fl) {
  out_dir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  out_fn <- paste0(glmtools::get_nml_value(nml_obj, 'out_fn'), '.nc')
  nc_filepath = file.path(sim_lake_dir, out_dir, out_fn)
  lake_depth <- glmtools::get_nml_value(nml_obj, arg_name = 'lake_depth')
  export_depths <- seq(0, lake_depth, by = 0.5)
  temp_data <- glmtools::get_temp(nc_filepath, reference = 'surface', z_out = export_depths) %>%
    mutate(date = as.Date(lubridate::floor_date(DateTime, 'days'))) %>% select(-DateTime)
  glmtools::get_var(nc_filepath, var_name = 'hice') %>%
    dplyr::mutate(ice = hice > 0, date = as.Date(lubridate::ceiling_date(DateTime, ' days'))) %>% dplyr::select(-hice, -DateTime) %>%
    dplyr::left_join(temp_data, ., by = 'date') %>%
    select(time = date, everything()) %>%
    feather::write_feather(export_fl)
}

#' @title run glm3 simulation
#' @description For each lake - gcm - time period combo, write the nml
#'  file, the meteo data, and run the glm3 model
#'  @param sim_dir base directory for simulations
#'  @param nml_objs list of nml objects (one per lake id), named by lake id
#'  @param meteo_xwalk a lake-gcm-time_period crosswalk table with the 
#'  meteo file name and hash for each model run
run_glm3_model <- function(sim_dir, nml_objs, meteo_xwalk, export_fl_template) {
  # pull lake_id from meteo_xwalk
  lake_id <- meteo_xwalk$site_id
  time_period <- meteo_xwalk$time_period
  gcm <- meteo_xwalk$gcm
  
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
  meteo_data <- munge_meteo_w_burn_in_out(meteo_xwalk, time_period_begin, time_period_end, 
                                          burn_in = 300, burn_out = 190)
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
  nml_obj$light$Kw_file <- NULL # use static value instead of relying on a file
  glmtools::write_nml(nml_obj, file.path(sim_lake_dir, 'glm3.nml'))
  
  # Set up export tibble
  export_fl <- sprintf(export_fl_template, lake_id, gcm, time_period)
  export_tibble <- tibble(
    run_date = Sys.time(),
    lake_id = lake_id,
    gcm = gcm,
    time_period = time_period,
    export_fl = export_fl)
  
  # for each model run, try running the model up to 5 times
  # if model run works, report how long that model took to run
  # and extract output and save to feather export file
  # return tibble with export file name and hash
  # if model run fails after 5 attempts, return
  # tibble indicating that it failed and giving the code
  tryCatch(
    {
      retry::retry(
        {glm_time <- system.time({glm_code <- GLM3r::run_glm(sim_lake_dir, verbose = FALSE)})[['elapsed']]},
        until=function(val, cnd) glm_code==0,
        max_tries = 5)
      
      # make sure glm did succeed
      if(glm_code != 0) stop()
      
      # extract output
      extract_glm_output(sim_lake_dir, nml_obj, export_fl)
      
      # Add glm run information to export tibble
      export_tibble <- export_tibble %>%
        mutate(export_hash = tools::md5sum(export_fl),
               glm_time_s = glm_time,
               glm_success = TRUE,
               glm_code = glm_code)
      return(export_tibble)
    },
    error = function(e) {
      glm_time <- system.time({glm_code <- GLM3r::run_glm(sim_lake_dir, verbose = FALSE)})[['elapsed']]
      
      # Make sure glm did indeed fail
      if(glm_code == 0) stop()
      
      # Add glm run information to export tibble
      export_tibble <- export_tibble %>%
        mutate(export_hash = tools::md5sum(export_fl),
               glm_time_s = glm_time,
               glm_success = FALSE,
               glm_code = glm_code)
      return(export_tibble)
    }
  )
}
