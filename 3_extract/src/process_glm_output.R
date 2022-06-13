#' @title Filter and write output from GLM model runs
#' @description function to read in the raw output from successful
#' GLM runs using GCM or NLDAS drivers, remove the ice thickness, 
#' evaporation, and n_layers variables (leaving the temperature 
#' predictions and ice flags), filter that output to exclude the
#' burn-in and burn-out periods, and save the result as a single 
#' feather file
#' @param run_group a single group of successful model runs. If the
#' passed runs are GCM runs, this group includes all runs for a lake-gcm combo
#' (nrows = 3). If the passed runs are NLDAS runs, this group includes all
#' runs for a given lake (nrows = 1). The function maps over these groups.
#' @param outfile_template the template for the name of the
#' final output feather file
#' @return a single feather file with the output temperature predictions
#' and ice flags for that lake-gcm combo (if GCM runs) or lake (if NLDAS runs), 
#' filtered to the valid dates (excluding burn-in/out from each time period)
write_glm_output <- function(run_group, outfile_template) {
  # set filename
  outfile <- sprintf(outfile_template, unique(run_group$site_id), unique(run_group$driver))
  
  # combine into single feather file and write,
  # saving only the temperature predictions and ice flags, and
  # truncating output for each time period to valid dates
  # (excluding burn-in and burn-out periods)
  purrr::pmap_df(run_group, function(...) {
    current_run <- tibble(...)
    # read in data for that time period, remove the ice
    # thickness, evaporation, and n_layers variables, and truncate
    # the predictions based on the defined start and end dates
    arrow::read_feather(current_run$export_fl) %>%
      select(-hice, -evap, -n_layers) %>%
      filter(time >= current_run$driver_start_date & time <= current_run$driver_end_date)
  }) %>% 
    arrow::write_feather(outfile)
  
  return(outfile)
}

#' @title Generate a tibble of information about the output
#' feather files
#' @description Generate a tibble with a row for each output file
#' that includes its filename and its hash along with the site_id, 
#' driver, the state the lake is in, and, if the passed runs are
#' GCM runs, the GCM spatial_cell_no, spatial_tile_no, data_cell_no, 
#' and data_tile_no for that lake.
#' @param output_feathers The file names of the final output feather files
#' returned by `write_glm_output()`, for all successfully modeled sites. If
#' the passed runs are GCM runs, each site has 6 output feather files. If
#' the passed runs are NLDAS runs, each site has 1 output feather file. 
#' The function maps over these file names.
#' @param output_site_ids vector of site_ids for which there are output
#' feather files
#' @param driver_names names of the driver(s) used to run the models. If
#' the passed runs are GCM runs, this is a vector of the 6 GCM names. If the
#' passed runs are NLDAS runs, this is 'NLDAS'
#' @param lake_xwalk - If the passed runs are GCM runs, this xwalk is a 
#' mapping of which lakes fall into which gcm cells and tiles (parameters 
#' `spatial_cell_no` and `spatial_tile_no`) and which gcm cell to use for 
#' driver data for each lake (`data_cell_no`, `data_tile_no`). `data_cell_no` 
#' will only differ from `spatial_cell_no` for those lakes that fall within gcm 
#' cells that are missing data. If the passed runs are NLDAS runs, this xwalk
#' is a mapping of which state each lake falls within.
#' @return A tibble with one row per output feather file which includes the 
#' site_id, driver, the name of the export feather file, its hash, the state 
#' the lake is in, and (if GCM output) the GCM spatial_cell_no, spatial_tile_no, 
#' data_cell_no, and data_tile_no for that lake.
generate_output_tibble <- function(output_feathers, output_site_ids, driver_names, lake_xwalk) {
  # collapse the site_ids and driver names vectors for use in string matching
  site_id_list <- paste(output_site_ids,collapse="|")
  driver_name_list <- paste(driver_names,collapse="|")
  
  # Build tibble of output files, hashes, driver, and site_id
  export_tibble <- tibble(
    export_fl = output_feathers,
    export_fl_hash = tools::md5sum(output_feathers),
    site_id = stringr::str_extract(export_fl, site_id_list),
    driver = stringr::str_extract(export_fl, driver_name_list)
  ) %>%
    select(site_id, driver, export_fl, export_fl_hash) %>%
    left_join(lake_xwalk, by='site_id')
  
  return(export_tibble)
}

#' @title Zip up output from GLM model runs
#' @description function to zip up output feather files
#' @param files_to_zip A single group of output feather files. If for GCM
#' runs, these feather files are associated with one spatial tile. If for
#' NLDAS runs, these are all NLDAS output files
#' @param zipfile_out the name of the final zipped file of feather files
#' @return the name of the zipped file 
zip_output_files <- function(files_to_zip, zipfile_out) {
  # In order to use `zip`, you need to be in the same working directory as
  # the files you want to zip up.
  project_dir <- getwd()
  setwd(unique(dirname(files_to_zip)))
  zip::zip(file.path(project_dir, zipfile_out), basename(files_to_zip))
  setwd(project_dir)
  return(zipfile_out)
}