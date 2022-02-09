#' @title Combine output from GLM model runs
#' @description function to read in the raw output from the GLM
#' runs for each lake-gcm combo (set by grouping of 
#' `p2_glm_uncalibrated_run_groups`) for each time period, filter 
#' that output to the valid dates within each time period 
#' (excluding the burn-in and burn-out periods), and save the
#' result as a single feather file
#' @param run_groups a grouped version of the `p2_glm_uncalibrated_runs`
#' output tibble subset to the lake_id, gcm, time_period, raw_meteo_fl, 
#' export_fl, and export_fl_hash columns and grouped by lake_id and gcm.
#' The function maps over these groups.
#' @param outfile_template the template for the name of the
#' final output feather file
#' @return the name of the output feather file
combine_glm_output <- function(run_groups, outfile_template) {
  # set filename
  outfile <- sprintf(outfile_template, unique(run_groups$lake_id), unique(run_groups$gcm))
  
  # combine into single feather file and write
  # truncating output for each time period to valid dates
  # (excluding burn-in and burn-out periods)
  purrr::map2_df(run_groups$raw_meteo_fl, run_groups$export_fl, function(raw_meteo_fl, export_file) {
    # Define time period begin and end dates from raw meteo_fl
    meteo_data <- arrow::read_feather(raw_meteo_fl, col_select = "time")
    begin <- min(meteo_data$time)
    end <- max(meteo_data$time)
    # read in data for that time period and truncate
    arrow::read_feather(export_file) %>%
      filter(time >= as.Date(begin) & time <= as.Date(end))
  }) %>% arrow::write_feather(outfile)
  
  return(outfile)
}
