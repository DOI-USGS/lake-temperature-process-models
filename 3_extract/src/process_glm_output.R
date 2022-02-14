#' @title Combine output from GLM model runs
#' @description function to read in the raw output from the 3 GLM
#' runs for each fully successful lake-gcm combo (set by grouping of 
#' `p2_glm_uncalibrated_run_groups`), filter that output to the 
#' burn-out periods, and save the result as a single feather file
#' @param run_group a single group from the `p2_glm_uncalibrated_run_groups`
#' grouped version of the `p2_glm_uncalibrated_runs` output tibble subset 
#' to the site_id, gcm, time_period, raw_meteo_fl, export_fl, and 
#' export_fl_hash columns and grouped by site_id and gcm. Then filtered to 
#' only groups for which glm_success==TRUE for all runs in that group. 
#' The function maps over these groups.
#' @param outfile_template the template for the name of the
#' final output feather file
#' @return a single feather file with the output data for that lake-gcm
#' combo, filtered to the valid dates from each time period
combine_glm_output <- function(run_group, outfile_template) {
  # set filename
  outfile <- sprintf(outfile_template, unique(run_group$site_id), unique(run_group$gcm))
  
  # combine into single feather file and write
  # truncating output for each time period to valid dates
  # (excluding burn-in and burn-out periods)
  purrr::map2_df(run_group$raw_meteo_fl, run_group$export_fl, function(raw_meteo_fl, export_file) {
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

#' @title Generate a tibble of information about the output
#' feather files
#' @description Generate a tibble with a row for each output file
#' (unique to each lake-gcm combo) that includes its filename and its 
#' hash along with the site_id, gcm, the state the lake is in, and 
#' the cell_no and tile_no for that lake. Use the lake_cell_tile_xwalk 
#' to determine which tile each lake falls into
#' @param run_group a single group from the `p2_glm_uncalibrated_run_groups`
#' grouped version of the `p2_glm_uncalibrated_runs` output tibble subset 
#' to the site_id, gcm, time_period, raw_meteo_fl, export_fl, and 
#' export_fl_hash columns and grouped by site_id and gcm. Then filtered to 
#' only groups for which glm_success==TRUE for all runs in that group. 
#' The function maps over these groups.
#' @param output_feather A single feather file name from the list of 
#' output feather file names returned by `combine_glm_output()`. 
#' The function maps over these file names.
#' @param lake_cell_tile_xwalk - mapping of which lakes fall into which 
#' gcm cells and tiles
#' @return A tibble with one row per lake-gcm output feather file which 
#' includes the site_id, gcm, the name of the export feather file, its 
#' hash, the state the lake is in, and the cell_no and tile_no for the 
#' GCM data for that lake. 
generate_output_tibble <- function(run_group, output_feather, lake_cell_tile_xwalk) {
  export_tibble <- tibble(
      site_id = unique(run_group$site_id),
      gcm = unique(run_group$gcm),
      export_fl = output_feather,
      export_fl_hash = tools::md5sum(output_feather)
    ) %>% 
    left_join(lake_cell_tile_xwalk, by='site_id')
  
  return(export_tibble)
}

#' @title Zip up output from GLM model runs
#' @description function to zip up the feather files for all lake-gcm
#' combos according to the tile_no associated with each lake
#' @param feather_group A single group of output feather files associated 
#' with one tile, from the `p3_glm_uncalibrated_output_feather_groups` 
#' grouped version of the `p3_glm_uncalibrated_output_feather_tibble` 
#' target grouped by tile_no. The function maps over these groups.
#' @param zipfile_template the template for the name of the
#' final zipped file of feather files
#' @return the name of the zipped file 
zip_output_files <- function(feather_group, zipfile_template) {
  files_to_zip <- feather_group$export_fl
  zipfile_out <- sprintf(zipfile_template, unique(feather_group$tile_no))
  # In order to use `zip`, you need to be in the same working directory as
  # the files you want to zip up.
  project_dir <- getwd()
  setwd(unique(dirname(files_to_zip)))
  zip::zip(file.path(project_dir, zipfile_out), basename(files_to_zip))
  setwd(project_dir)
  return(zipfile_out)
}