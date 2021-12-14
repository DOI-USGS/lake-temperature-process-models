#' @Title run glm3 simulation
#' @description For each lake - gcm - time period combo, write the nml
#'  file, the meteo data, and run the glm3 model
#'  @param sim_dir base directory for simulations
#'  @param nml_objs list of nml objects (one per lake id), named by lake id
#'  @param meteo_xwalk a lake-gcm-time_period crosswalk table with the 
#'  meteo file name and hash for each model run
run_glm3_model <- function(sim_dir, nml_objs, meteo_xwalk) {
  # pull lake_id from meteo_xwalk
  lake_id <- meteo_xwalk$site_id
  time_period <- meteo_xwalk$time_period
  gcm <- meteo_xwalk$gcm
  
  # prepare to write inputs and results locally for quick I/O
  sim_lake_dir <- file.path(sim_dir, lake_id, gcm, time_period)
  dir.create(sim_lake_dir, recursive=TRUE, showWarnings=FALSE)
  # FOR NOW - commenting out code to delete directory after model execution
  # since not yet extracting output of model into feather file
  # on.exit(unlink(sim_lake_dir, recursive = TRUE))
  
  # Define data start and stop dates
  times <- strsplit(time_period,'_')[[1]]
  start <- sprintf('%s-01-01', times[1])
  stop <- sprintf('%s-12-31', times[2])
  
  # Set up burn in and burn out
  tmp_data <- arrow::read_feather(meteo_xwalk$meteo_fl) %>%
    mutate(time = as.Date(time)) %>% filter(time >= as.Date(start) & time <= as.Date(stop)) %>%
    mutate(Rain = case_when(Snow > 0 ~ 0, TRUE ~ Rain))
  burn_in <- 300 # days
  burn_out <- 190 # days
  # create a burn-in. We're going to mirror the data of the first year:
  burn_in_data <- tmp_data[2:burn_in, ] %>% arrange(desc(time)) %>%
    mutate(time = seq(from = tmp_data[1,]$time-burn_in+1, by = 'day', length.out = length(2:burn_in)))
  # bind with real data, prior to the start. We're not duplicating dates, hence the "2"
  tmp_data <- bind_rows(burn_in_data, tmp_data)
  # create a burn-out. Mirror the final data:
  burn_out_data <- tail(tmp_data, burn_out) %>% head(-1L) %>% arrange(desc(time)) %>%
    mutate(time = seq(from = tail(tmp_data,1)$time+1, by = 'day', length.out = length(2:burn_out)))
  # bind with real data, at the end of the sequence
  tmp_data <- bind_rows(tmp_data, burn_out_data)
  # write to the simulation file
  readr::write_csv(tmp_data, file.path(sim_lake_dir, 'meteo_fl.csv'))
  
  # write nml file, specifying meteo file and start and stop dates:
  nml_obj <- nml_objs[[lake_id]]
  # out_fn <- paste0(glmtools::get_nml_value(nml_obj, 'out_fn'), '.nc')
  nml_obj <- set_nml(nml_obj, arg_list = list(nsave = 24, meteo_fl = 'meteo_fl.csv',
                                              lake_name = lake_id,
                                              stop = format(tail(tmp_data, 1L)$time, "%Y-%m-%d"), 
                                              start = format(tmp_data[1,]$time, "%Y-%m-%d")))
  nml_obj$light$Kw_file <- NULL # use static value instead of relying on a file
  glmtools::write_nml(nml_obj, file.path(sim_lake_dir, 'glm3.nml'))
  
  # run once with hourly output:
  # FOR NOW - running w/ Alison's code to help debug
  # eventually, will just run with:
  # GLM3r::run_glm(sim_lake_dir, verbose = FALSE)
  glm_time <- system.time({
    glm_code <- tryCatch(
      {
        try1_code <- GLM3r::run_glm(sim_lake_dir, verbose = FALSE)
        if(try1_code != 0) stop()
        try1_code
      },
      error = function(e) {
        message('GLM failed; retrying with verbose = TRUE')
        try2_code <- GLM3r::run_glm(sim_lake_dir, verbose = TRUE)
        try2_code
      })
  })[['elapsed']]
  message(sprintf('GLM ran in %0.0f:%02.0f (M:S)', floor(glm_time/60), glm_time %% 60))
  if(glm_code != 0) {
    warning(sprintf("GLM code for %s suggests a problem: %s", lake_id, glm_code))
    return(tibble(
      run_date = Sys.time(),
      lake_id = lake_id,
      gcm = gcm,
      time_period = time_period,
      glm_time_s = glm_time,
      glm_success = FALSE,
      glm_code = glm_code,
      glm_hash = NA))
  }

  # FOR NOW
  # return a 1-row tibble of information about this model run
  return(tibble(
    run_date = Sys.time(),
    lake_id = lake_id,
    gcm = gcm,
    time_period = time_period,
    glm_time_s = glm_time,
    glm_success = TRUE,
    glm_code = glm_code,
    glm_hash = dir(file.path(sim_lake_dir, 'out'), full.names = TRUE) %>%
      purrr::map_chr(tools::md5sum) %>% # hash each of the output files in the sim directory
      digest::digest('sha1') # combine the file hashes into a single object hash
  ))
  
  # TODO - extract output of model run and save in feather file
  # In separate step, output for each lake, for each gcm, will
  # be combined into a single feather file (one w/ output for all 3 time periods)
}