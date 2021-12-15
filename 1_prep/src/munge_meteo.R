#' @title Split each GCM netCDF into feather files specific to cells and time periods
#' @decription Create a feather file with driver data for each gcm, 
#' for each cell, for each time period
#' @param gcm_nc filenames of GCM netCDF files
#' @param gcm_name name of one of the six GCMs
#' @param cell_id id of a single GCM cell that is known to contain lakes
#' @param gcm_date bracketing years of a single GCM time period
#' @param outfile_template string representing the filepath at which to save
#' the feather file output of the split netCDF data. The first `%s` is
#' used as the placeholder for the `gcm_name`, the second is for the `gcm_date`,
#' and the third is for the `cell_no`.
#' @return names of cell/gcm/time-period specific feather files
munge_nc_files <- function(gcm_nc, gcm_name, cell_no, gcm_date, outfile_template) {
  # open netCDF file (gcm-specific) and pull data for that cell, for that time period
  # TODO - actually pull time data for only one time period - pulling all for now
  nc <- nc_open(gcm_nc)
  time_dim <- ncvar_get(nc, "time")
  cell_dim <- ncvar_get(nc, "gridcells")
  data <- purrr::map_df(nc$var, function(var) {
    ncvar_get(nc, var, start = c(1,cell_no), count = c(-1,1))
  }) %>% mutate(time = nc.get.time.series(f = nc,time.dim.name = "time"),.before=1)

  
  # write to feather file, specific to gcm, cell, and time period
  outfile <- sprintf(outfile_template, gcm_name, gcm_date, cell_no)
  if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive=TRUE)
  arrow::write_feather(data, outfile)
  
  return(outfile)
}

#' @title build a lake-gcm-time-period crosswalk table
#' @description Build a dplyr table with tar_grouping that has one row per
#' Lake-GCM-time-period combination and columns for the (1) tar branch name 
#' and (2) data hash for the meteo data for each combo. We can use this to run models.
#' For the string extraction to work, cell_no must be at the END of the meteo filename
#' @param meteo_feathers - names of the cell/gcm/time-period specific feather files
#' @param lake_cell_xwalk - mapping of which lakes fall into which gcm cells
#' @param gcm_names - names of the 6 GCMs
#' @param gcm_dates - the three GCM time periods, defined by their bracketing years
#' @return a dplyr tibble with one row per model run
build_meteo_xwalk <- function(meteo_feathers, lake_cell_xwalk, gcm_names, gcm_dates){
  # collapse the gcm_names, gcm_date, and cell_no vectors for use in string matching
  gcm_name_list <- paste(gcm_names,collapse="|")
  gcm_date_list <- paste(gcm_dates,collapse="|")
  cell_no_list <- paste(unique(lake_cell_xwalk$cell_no),collapse= "|")
  # Build tibble of meteo files, branches, hashes, gcm name, cell_no, and time_period
  meteo_branches <- tibble(
    meteo_fl = meteo_feathers,
    meteo_branch = names(meteo_feathers),
    meteo_data = map_chr(meteo_branch, ~ tar_meta(starts_with(.x))$data),
    gcm = stringr::str_extract(meteo_fl, gcm_name_list),
    time_period = stringr::str_extract(meteo_fl, gcm_date_list),
    # cell_no is the very last part of the file name and so this str_extract() is 
    # set up to use a regex that looks for a match in the last part of the filename 
    # just before the file extension
    cell_no = as.numeric(str_extract(tools::file_path_sans_ext(meteo_fl), paste0(cell_no_list, "$")))
  )
  meteo_xwalk <- tidyr::expand_grid(
      nesting(select(lake_cell_xwalk, site_id, ID, cell_no)),
      gcm = gcm_names, 
      time_period = gcm_dates) %>%
    dplyr::relocate(c(gcm, time_period), .before=cell_no) %>%
    arrange(site_id) %>%
    left_join(meteo_branches, by=c('gcm', 'cell_no', 'time_period')) %>%
    rowwise() %>%
    tar_group()
  return(meteo_xwalk)
}