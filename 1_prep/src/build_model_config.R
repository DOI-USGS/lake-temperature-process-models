#' @title build a lake-gcm-time-period crosswalk table that serves as the model config
#' @description Build a dplyr table with tar_grouping that has one row per
#' Lake-GCM-time-period combination and columns for the (1) tar branch name 
#' and (2) data hash for the meteo data for each combo. We can use this to run models.
#' For the string extraction to work, data_cell_no must be at the END of the meteo filename
#' @param meteo_feathers - names of the gcm/time-period/cell specific feather files
#' @param lake_cell_tile_xwalk - mapping of which lakes fall into which gcm cells and tiles
#' (parameters `spatial_cell_no` and `spatial_tile_no`) and which gcm cell to use for driver 
#' data for each lake (`data_cell_no`,`data_tile_no`). `data_cell_no` will only differ from
#' `spatial_cell_no` for those lakes that fall within gcm cells that are missing data.
#' @param gcm_names - names of the 6 GCMs
#' @param gcm_dates - the three GCM time periods, defined by their bracketing years
#' @return a dplyr tibble with one row per model run
build_model_config <- function(meteo_feathers, lake_cell_tile_xwalk, gcm_names, gcm_dates){
  # collapse the gcm_names, gcm_date, and cell_no vectors for use in string matching
  gcm_name_list <- paste(gcm_names,collapse="|")
  gcm_date_list <- paste(gcm_dates,collapse="|")
  data_cell_no_list <- paste(unique(lake_cell_tile_xwalk$data_cell_no),collapse= "|")
  # Build tibble of meteo files, branches, hashes, gcm name, cell_no, and time_period
  meteo_branches <- tibble(
    meteo_fl = meteo_feathers,
    meteo_fl_hash = tools::md5sum(meteo_feathers),
    gcm = stringr::str_extract(meteo_fl, gcm_name_list),
    time_period = stringr::str_extract(meteo_fl, gcm_date_list),
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
    time_period = gcm_dates) %>%
    arrange(site_id) %>%
    left_join(meteo_branches, by=c('gcm', 'data_cell_no', 'time_period')) %>%
    select(-data_cell_no) %>%
    rowwise()
  return(model_config)
}
