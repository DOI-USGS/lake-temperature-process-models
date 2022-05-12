#' @title build a lake-gcm-time-period crosswalk table that serves as the model config
#' @description Build a dplyr table with tar_grouping that has one row per
#' Lake-GCM-time-period combination and columns for the (1) file name 
#' and (2) data hash for the meteo data for each combo. We can use this to run models.
#' For the string extraction to work, data_cell_no must be at the END of the meteo filename
#' @param gcm_csvs - names of the gcm/time-period/cell specific csv files
#' @param lake_cell_tile_xwalk - mapping of which lakes fall into which gcm cells and tiles
#' (parameters `spatial_cell_no` and `spatial_tile_no`) and which gcm cell to use for driver 
#' data for each lake (`data_cell_no`,`data_tile_no`). `data_cell_no` will only differ from
#' `spatial_cell_no` for those lakes that fall within gcm cells that are missing data.
#' @param gcm_names - names of the 6 GCMs
#' @param gcm_dates - a tibble with a row for each time period, and columns specifying
#' the start and end date of each time period, as well as the length of
#' burn in, the burn-in start date, the length of burn-out, and the burn-out
#' end date for each time period
#' @return a dplyr tibble with one row per model run, with columns for site_id, state,
#' driver (name of gcm), time_period, driver_start_date, driver_end_date, burn_in, 
#' burn_in_start, burn_out, burn_out_end, meteo_fl, and meteo_fl_hash
build_gcm_model_config <- function(gcm_csvs, lake_cell_tile_xwalk, gcm_names, gcm_dates){
  # collapse the gcm_names, gcm_date, and cell_no vectors for use in string matching
  gcm_name_list <- paste(gcm_names,collapse="|")
  gcm_time_period_list <- paste(gcm_dates$time_period,collapse="|")
  data_cell_no_list <- paste(unique(lake_cell_tile_xwalk$data_cell_no),collapse= "|")
  # Build tibble of meteo files, branches, hashes, gcm name, cell_no, and time_period
  meteo_branches <- tibble(
    meteo_fl = gcm_csvs,
    meteo_fl_hash = tools::md5sum(gcm_csvs),
    driver = stringr::str_extract(meteo_fl, gcm_name_list),
    time_period = stringr::str_extract(meteo_fl, gcm_time_period_list),
    # cell_no is the very last part of the file name and so this str_extract() is 
    # set up to use a regex that looks for a match in the last part of the filename 
    # just before the file extension
    data_cell_no = as.numeric(str_extract(tools::file_path_sans_ext(meteo_fl), paste0(data_cell_no_list, "$")))
  )
  # build model config table, with one row per lake-gcm-time-period combination
  # and the `meteo_fl` filename and `meteo_fl_hash` for each model run. We join the
  # meteo files to the xwalk using the `data_cell_no` field b/c that field indicates
  # which gcm cell to use for driver data for each lake. Leave out `data_cell_no`
  # from the final tibble since it is not used during model execution and if changed,
  # should not trigger rebuilds unless the underlying data changes, which is 
  # captured by the `meteo_fl_hash` column
  model_config <- tidyr::expand_grid(
    nesting(select(lake_cell_tile_xwalk, site_id, state, data_cell_no)),
    driver = gcm_names, 
    nesting(gcm_dates)) %>%
    arrange(site_id) %>%
    left_join(meteo_branches, by=c('driver', 'data_cell_no', 'time_period')) %>%
    select(-data_cell_no) %>%
    rowwise()
  return(model_config)
}

#' @title build a model configuration table for the NDLAS runs
#' @description Build a dplyr table with tar_grouping that has one row per
#' lake and columns for the (1) file name and (2) data hash for the meteo data for each 
#' lake. We can use this to run models.
#' @param site_ids - The vector of site_ids (p1_site_ids)
#' @param nml_list - nested list of lake-specific nml parameters
#' @param nldas_csvs - The unique nldas files (p1_nldas_csvs)
#' @param nldas_dates - a tibble with a row for the 1980-2021 NLDAS time period, and 
#' columns specifying the start and end date of the time period, as well as the length of
#' burn in, the burn-in start date, the length of burn-out, and the burn-out
#' end date for each time period
#' @return a dplyr tibble with nrows = length(p1_site_ids) with columns for site_id, 
#' driver ('NLDAS'), time_period, driver_start_date, driver_end_date, burn_in, burn_in_start, 
#' burn_out, burn_out_end, meteo_fl, and meteo_fl_hash
build_nldas_model_config <- function(site_ids, nml_list, nldas_csvs, nldas_dates) {
  meteo_branches <- tibble(
    meteo_fl = nldas_csvs,
    basename_meteo_fl = basename(meteo_fl),
    meteo_fl_hash = tools::md5sum(nldas_csvs)
  )
  site_meteo_xwalk <- purrr::map_df(site_ids, function(site_id) {
    site_nml_list <- nml_list[[site_id]]
    site_meteo_xwalk <- tibble(
      site_id = site_id,
      basename_meteo_fl = site_nml_list$meteo_fl
    )
    return(site_meteo_xwalk)
  })
  model_config <- tidyr::expand_grid(
    nesting(site_meteo_xwalk),
    driver = 'NLDAS', 
    nesting(nldas_dates)) %>%
    arrange(site_id) %>%
    left_join(meteo_branches, by=c('basename_meteo_fl')) %>%
    select(-basename_meteo_fl) %>%
    rowwise()
  
  return(model_config)
}
