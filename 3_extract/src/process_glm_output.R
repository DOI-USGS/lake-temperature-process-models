#' @title Combine output from GLM model runs
#' @description function to read in the raw output from the GLM
#' runs for each lake-gcm combo (set by grouping of 
#' `p2_glm_uncalibrated_run_groups`) for each time period, filter 
#' that output to the valid dates within each time period 
#' (excluding the burn-in and burn-out periods), and save the
#' result as a single feather file
#' @param run_groups a grouped version of the `p2_glm_uncalibrated_runs`
#' output tibble subset to the site_id, gcm, time_period, raw_meteo_fl, 
#' export_fl, and export_fl_hash columns and grouped by site_id and gcm.
#' Then filtered to only groups for which glm_success==TRUE for all runs
#' in that group. The function maps over these groups.
#' @param lake_cell_tile_xwalk - mapping of which lakes fall into which 
#' gcm cells and tiles
#' @param outfile_template the template for the name of the
#' final output feather file
#' @return a tibble with one row per lake-gcm combo which includes the 
#' site_id, gcm, the name of the export feather file, its hash, the state 
#' the lake is in, and the cell_no and tile_no for the GCM data for that lake. 
combine_glm_output <- function(run_groups, lake_cell_tile_xwalk, outfile_template) {
  # set filename
  outfile <- sprintf(outfile_template, unique(run_groups$site_id), unique(run_groups$gcm))
  
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
  
  export_tibble <- tibble(
    site_id = unique(run_groups$site_id),
    gcm = unique(run_groups$gcm),
    export_fl = outfile,
    export_fl_hash = tools::md5sum(outfile),
  ) %>%
    left_join(lake_cell_tile_xwalk, by=c('site_id'))
  return(export_tibble)
}
