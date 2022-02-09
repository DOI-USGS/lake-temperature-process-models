#' @title build a lake-gcm-time-period crosswalk table that serves as the model config
#' @description Build a dplyr table with tar_grouping that has one row per
#' Lake-GCM-time-period combination and columns for the (1) tar branch name 
#' and (2) data hash for the meteo data for each combo. We can use this to run models.
#' For the string extraction to work, cell_no must be at the END of the meteo filename
#' @param meteo_feathers - names of the cell/gcm/time-period specific feather files
#' @param lake_cell_tile_xwalk - mapping of which lakes fall into which gcm cells and tiles
#' @param gcm_names - names of the 6 GCMs
#' @param gcm_dates - the three GCM time periods, defined by their bracketing years
#' @return a dplyr tibble with one row per model run
build_model_config <- function(meteo_feathers, lake_cell_tile_xwalk, gcm_names, gcm_dates){
  # collapse the gcm_names, gcm_date, and cell_no vectors for use in string matching
  gcm_name_list <- paste(gcm_names,collapse="|")
  gcm_date_list <- paste(gcm_dates,collapse="|")
  cell_no_list <- paste(unique(lake_cell_tile_xwalk$cell_no),collapse= "|")
  # Build tibble of meteo files, branches, hashes, gcm name, cell_no, and time_period
  meteo_branches <- tibble(
    meteo_fl = meteo_feathers,
    meteo_fl_hash = map_chr(names(meteo_feathers), ~ tar_meta(starts_with(.x))$data),
    gcm = stringr::str_extract(meteo_fl, gcm_name_list),
    time_period = stringr::str_extract(meteo_fl, gcm_date_list),
    # cell_no is the very last part of the file name and so this str_extract() is 
    # set up to use a regex that looks for a match in the last part of the filename 
    # just before the file extension
    cell_no = as.numeric(str_extract(tools::file_path_sans_ext(meteo_fl), paste0(cell_no_list, "$")))
  )
  model_config <- tidyr::expand_grid(
    nesting(select(lake_cell_tile_xwalk, site_id, state, cell_no)),
    gcm = gcm_names, 
    time_period = gcm_dates) %>%
    dplyr::relocate(c(gcm, time_period), .before=cell_no) %>%
    arrange(site_id) %>%
    left_join(meteo_branches, by=c('gcm', 'cell_no', 'time_period')) %>%
    rowwise()
  return(model_config)
}
