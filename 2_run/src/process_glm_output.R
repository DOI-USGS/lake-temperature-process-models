#' @title Combine output from GLM model runs
#' @description function to read in the raw output from the GLM
#' runs for each lake-gcm combo (set by grouping of 
#' `p2_glm_uncalibrated_run_groups`) for each time period, filter 
#' that output to the valid dates within each time period 
#' (excluding the burn-in and burn-out periods), and save the
#' result as a single feather file
#' @param run_groups a grouped version of the `p2_glm_uncalibrated_runs`
#' output tibble subset to the lake_id, gcm, time_period, export_fl, 
#' and export_fl_hash columns and grouped by lake_id and gcm.
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
  purrr::map2_df(run_groups$export_fl, run_groups$time_period, function(export_file, time_period) {
    # Define time period begin and end dates
    times <- strsplit(time_period,'_') %>% unlist()
    begin <- sprintf('%s-01-01', times[1])
    end <- sprintf('%s-12-31', times[2])
    # read in data for that time period and truncate
    arrow::read_feather(export_file) %>%
      filter(time >= as.Date(begin) & time <= as.Date(end))
  }) %>% arrow::write_feather(outfile)
  
  return(outfile)
}
