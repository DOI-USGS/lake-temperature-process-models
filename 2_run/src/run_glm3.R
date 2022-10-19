#' @title function to extract and write glm output following a model run
#' @description Extract temperature, layers, evaporation, and ice data 
#' from the model run and save to the export feather file. 
#' Adapted from Jordan's GLM projection code: 
#' https://github.com/jread-usgs/lake-temperature-process-models/blob/master/3_run/src/run_glm_utils.R#L89-L101
#' @param nc_filepath the path for the netCDF output file for the simulation
#' @param nml_obj the complete nml object for the model run
#' @param export_fl the filepath for the exported feather file
extract_glm_output <- function(nc_filepath, nml_obj, export_fl) {
  lake_depth <- glmtools::get_nml_value(nml_obj, arg_name = 'lake_depth')
  export_depths <- seq(0, lake_depth, by = 0.5)
  
  # Extract temperature predictions for simulation
  # This may trigger an error ('need at least two non-NA values to interpolate')
  # if the temperature predictions contain NA values. If so, that's documented
  # as a 'NA temperature values' error in our export tibble returned by `run_glm3_model()`
  temp_data <- glmtools::get_temp(nc_filepath, reference = 'surface', z_out = export_depths) %>%
    mutate(time = as.Date(lubridate::floor_date(DateTime, 'days'))) %>% 
    select(-DateTime)
  
  # Extract layers, evaporation, and ice data for simulation
  layers_data <- glmtools::get_var(nc_filepath, 'NS') %>%
    rename(n_layers = NS) %>%
    mutate(time = as.Date(lubridate::ceiling_date(DateTime, ' days'))) %>%
    select(-DateTime)
  evap_data <- glmtools::get_var(nc_filepath, 'evap') %>%
    mutate(time = as.Date(lubridate::ceiling_date(DateTime, ' days'))) %>%
    select(-DateTime)
  ice_data <- glmtools::get_var(nc_filepath, var_name = 'hice') %>%
    mutate(ice = hice > 0, time = as.Date(lubridate::ceiling_date(DateTime, ' days'))) %>%
    select(-DateTime)
  
  # As a further check of whether or not the model run was successful, 
  # check if # layers = 1 at any point during the simulation
  error_message <- NA
  min_layers <- min(layers_data$n_layers)
  if (min_layers == 1) {
    error_message <- 'Number of layers drops to 1'
  }
  # also check if max ice thickness exceeds lake depth
  max_hice <- max(ice_data$hice)
  if (max_hice > lake_depth) {
    error_message <- ifelse(is.na(error_message), 'Maximum hice value exceeds lake depth', sprintf('%s, Maximum hice value exceeds lake depth', error_message)) 
  }
  # If `error_message` is not NA, trigger an error and return the error message
  if (!is.na(error_message)) {
    stop(error_message)
  }

  all_results <- temp_data %>%
    left_join(ice_data, by = 'time') %>%
    left_join(evap_data, by = 'time') %>%
    left_join(layers_data, by = 'time') %>%
    select(time, everything()) %>%
    arrow::write_feather(export_fl)
}

#' @title run glm3 simulation
#' @description For each lake - driver - time period combo, write the nml
#' file, the meteo data, and run the glm3 model. If the model run is
#' successful, the results are extracted and saved to '2_run/tmp' using
#' `extract_glm_ouput()` and then the simulation directory is deleted 
#' @param sim_dir base directory for simulations
#' @param nml_obj nml object for the current lake
#' @param model_config a model configuration table with one 
#' row per model run and columns for site_id, state, driver, time_period, 
#' driver_start_date, driver_end_date, burn_in, burn_in_start, burn_out, 
#' burn_out_end, meteo_fl, and meteo_fl_hash
#' @param export_fl_template the template for constructing the filepath 
#' for the export feather file that will be saved in `extract_glm_ouput()`
#' @return a tibble which includes the run_date, site_id, driver, time_period, 
#' the name of the export feather file, its hash (NA if the model run failed), 
#' the duration of the model run, whether or not the model run succeeded, 
#' and the code returned by the call to GLM3r::run_glm(). 
run_glm3_model <- function(sim_dir, nml_obj, model_config, export_fl_template) {
  # pull parameters from model_config
  site_id <- model_config$site_id
  time_period <- model_config$time_period
  driver <- model_config$driver
  raw_meteo_fl <- model_config$meteo_fl
  
  # prepare to write inputs and results locally for quick I/O
  # if the run simulation directory already exists (b/c previous run for site failed), 
  # delete the simulation directory before creating a new one, in order to have 
  # a clean record of the current run
  sim_lake_dir <- file.path(sim_dir, sprintf('%s_%s_%s', site_id, driver, time_period))
  if (dir.exists(sim_lake_dir)) unlink(sim_lake_dir, recursive = TRUE)
  dir.create(sim_lake_dir, recursive=TRUE, showWarnings=FALSE)
  
  # copy meteo data to sim_lake_dir
  sim_meteo_filename <- basename(raw_meteo_fl)
  file.copy(from = raw_meteo_fl, to = sim_lake_dir)
  
  # Define simulation start and stop dates based on burn-in and burn-out periods
  sim_start <- as.character(model_config$burn_in_start)
  sim_stop <- as.character(model_config$burn_out_end)
  
  # write nml file, specifying meteo file and start and stop dates:
  nml_obj <- set_nml(nml_obj, arg_list = list(nsave = 24, 
                                              meteo_fl = sim_meteo_filename,
                                              sim_name = sprintf('%s_%s_%s', site_id, driver, time_period),
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
        {
          # Attempt to run GLM and store the execution time
          glm_time <- system.time({glm_code <- GLM3r::run_glm(sim_lake_dir, verbose = FALSE)})[['elapsed']]
          # Pull out the final date from the output to
          # check against requested simulation end date
          # use 1D variable 'evap' rather than 2D 'temp' to check output dates because
          # 1) less expensive to pull and 2) defaults cause an error with glmtools::get_temp()
          # when the lake is too shallow.
          output_dates <- glmtools::get_var(nc_filepath, var_name = 'evap') %>%
            mutate(date = format(as.Date(lubridate::floor_date(DateTime, 'days')),"%Y-%m-%d")) %>%
            pull(date)
          max_output_date <- max(output_dates)
        },
        until=function(val, cnd) glm_code == 0 & max_output_date==sim_stop,
        max_tries = 2)
      
      # make sure glm did succeed  
      if(glm_code != 0 | max_output_date!=sim_stop) stop()
      
      # extract output
      export_fl <- sprintf(export_fl_template, site_id, driver, time_period)
      extraction_error <- tryCatch(
        {
          extract_glm_output(nc_filepath, nml_obj, export_fl)
          extraction_error <- NA
        },
        error = function(e) {
          if (grepl(e$message, 'need at least two non-NA values to interpolate')) {
            extraction_error <- 'NA temperature values'
          } else {
            extraction_error <- e$message
          }
          return(extraction_error)
        }
      )
      # If extraction error is NOT NA, trigger error
      # and return the error export_tibble
      if (!is.na(extraction_error)) stop()
      
      # If output was extracted, delete simulation directory
      # We are _not_ deleting the simulation directory for failed
      # runs so that we can explore the model output
      unlink(sim_lake_dir, recursive = TRUE)
      
      # Build export tibble with export file, its hash, and glm run information
      export_tibble <- tibble(
        site_id = site_id,
        driver = driver,
        time_period = time_period,
        raw_meteo_fl = raw_meteo_fl,
        driver_start_date = model_config$driver_start_date,
        driver_end_date = model_config$driver_end_date,
        burn_in = model_config$burn_in,
        burn_out = model_config$burn_out,
        export_fl = export_fl,
        export_fl_hash = tools::md5sum(export_fl),
        extraction_error = extraction_error,
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
        site_id = site_id,
        driver = driver,
        time_period = time_period,
        raw_meteo_fl = raw_meteo_fl,
        driver_start_date = model_config$driver_start_date,
        driver_end_date = model_config$driver_end_date,
        burn_in = model_config$burn_in,
        burn_out = model_config$burn_out,
        export_fl = NA,
        export_fl_hash = NA,
        extraction_error = ifelse(exists("extraction_error"), extraction_error, NA), # If the error happened prior to the attempt at extraction, need to provide an NA here
        glm_run_date = Sys.time(),
        glm_version = GLM3r::glm_version(as_char = TRUE), #Needs version 3.1.18 of GLM3r
        glm_time_s = glm_time,
        param_sim_start = sim_start,
        param_sim_stop = sim_stop,
        max_output_date = ifelse(exists("max_output_date"), max_output_date, NA), #Set to NA if couldn't be extracted
        glm_code = glm_code,
        glm_success = FALSE)
      return(export_tibble)
    }
  )
}
