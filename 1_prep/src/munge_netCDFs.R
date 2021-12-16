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
  arrow::write_feather(data, outfile)
  
  return(outfile)
}
