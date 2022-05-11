#' @title build a lake-gcm-time-period crosswalk table that serves as the model config
#' @description Build a dplyr table with tar_grouping that has one row per
#' Lake-GCM-time-period combination and columns for the (1) tar branch name 
#' and (2) data hash for the meteo data for each combo. We can use this to run models.
#' For the string extraction to work, data_cell_no must be at the END of the meteo filename
#' @param meteo_csvs - names of the gcm/time-period/cell specific csv files
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
#' gcm, time_period, gcm_start_date, gcm_end_date, burn_in, burn_in_start, burn_out, burn_out_end,
#' meteo_fl, and meteo_fl_hash
build_model_config <- function(meteo_csvs, lake_cell_tile_xwalk, gcm_names, gcm_dates){
  # collapse the gcm_names, gcm_date, and cell_no vectors for use in string matching
  gcm_name_list <- paste(gcm_names,collapse="|")
  gcm_time_period_list <- paste(gcm_dates$time_period,collapse="|")
  data_cell_no_list <- paste(unique(lake_cell_tile_xwalk$data_cell_no),collapse= "|")
  # Build tibble of meteo files, branches, hashes, gcm name, cell_no, and time_period
  meteo_branches <- tibble(
    meteo_fl = meteo_csvs,
    meteo_fl_hash = tools::md5sum(meteo_csvs),
    gcm = stringr::str_extract(meteo_fl, gcm_name_list),
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
    gcm = gcm_names, 
    nesting(gcm_dates)) %>%
    arrange(site_id) %>%
    left_join(meteo_branches, by=c('gcm', 'data_cell_no', 'time_period')) %>%
    select(-data_cell_no) %>%
    rowwise()
  return(model_config)
}

build_nldas_model_config <- function(site_id, nldas_csv, nldas_dates) {
  model_config <- tibble(
    site_id = site_id, 
    gcm = 'nldas', #using column name 'gcm' for testing - then will be 'driver_type'
    meteo_fl = nldas_csv,
    meteo_fl_hash = tools::md5sum(nldas_csv)
  ) %>%
    cbind(nldas_dates) %>%
    # for now, use column names 'gcm_start_date' and 'gcm_end_date' for testing, - then will be 'driver_start_date'
    select(site_id, gcm, time_period, gcm_start_date=driver_start_date, gcm_end_date=driver_end_date, 
           burn_in, burn_in_start, burn_out, burn_out_end, meteo_fl, meteo_fl_hash) #select(site_id, gcm, colnames(nldas_dates), meteo_fl, meteo_fl_hash)

  return(model_config)
}

#' @title Munge NLDAS dates
#' @description Function to determine the start and end dates of the
#' NLDAS time period and document the date when burn-in starts and the date
#' when burn-out ends, based on the dates of the raw NLDAS meteorological data.
#' @param nldas_csvs filepaths of the NLDAS csv files
#' @param nldas_time_period - the user-set NLDAS time period, defined by its 
#' bracketing years
#' @return a tibble with a row for each time period, and columns specifying
#' the start and end date of each time period, as well as the length of
#' burn in, the burn-in start date, the length of burn-out, and the burn-out
#' end date for each time period
munge_nldas_dates <- function(nldas_csvs, nldas_time_period) {
  model_years <- strsplit(nldas_time_period,'_')[[1]]
  
  # Use first csv file, since all have the same date range
  nldas_meteo <- readr::read_csv(nldas_csvs[1], col_types= cols())
  
  nldas_dates <- tibble(
    time = lubridate::as_date(nldas_meteo$time),
    time_period = nldas_time_period
    ) %>%
    group_by(time_period) %>%
    # set burn-in start and burn-out end based on extend of NLDAS data
    summarize(burn_in_start = min(time), burn_out_end = max(time)) %>%
    mutate(
      driver_start_date = as.Date(sprintf('%s-01-01', model_years[1])), # set based on user-defined modeling period
      driver_end_date = as.Date(sprintf('%s-12-31', model_years[2])), # set based on user-defined modeling period
      # the burn-in period is the period that precedes the first full year of the NLDAS time period
      burn_in = driver_start_date - burn_in_start,
      # the burn-out period is the period that follows the final full year of the NLDAS time period
      burn_out = burn_out_end - driver_end_date)
  
  return(nldas_dates)
}