#' @title Split each GCM netCDF into feather files specific to cells and time periods
#' @decription Create a feather file with driver data for the current gcm, for
#' each cell, for each time period
#' @param gcm_nc filename of a GCM netCDF file
#' @param gcm_name name of one of the six GCMs
#' @param cell_nos vector of GCM cell ids
#' @param outfile_template string representing the filepath at which to save
#' the feather file output of the split netCDF data. The first `%s` is
#' used as the placeholder for the `gcm_name`, the second is for the time period,
#' and the third is for the cell_no.
#' @return names of gcm/time-period/cell-specific feather files
munge_nc_files <- function(gcm_nc, gcm_name, cell_nos, outfile_template) {
  # Open the netCDF file for the current GCM
  nc <- ncdfgeom::read_timeseries_dsg(gcm_nc)
  
  # Pull the vectors of dates
  nc_dates <- lubridate::as_date(nc$time)
  
  # Generate a list of time-period specific and cell-specific files
  # For each cell...
  time_period_cell_files <- purrr::map(cell_nos, function(cell_no) {
    # For each variable stored in the netCDF...
    cell_data <- purrr::map2_dfc(nc$data_frames, names(nc$data_frames), function(var_df, var_name) {
      # Pull the column pertaining to the current cell and rename it to the variable name
      var_cell_long <- var_df %>%
        select(as.character(cell_no)) %>%
        rename(!!var_name := as.character(cell_no))
    }) %>%
      # Once all variables have been compiled for each cell,
      # Determine the time period for each row
      mutate(time = nc_dates,
             time_period = case_when(
               time <= as.Date("2010-01-01") ~ "1980_1999",
               time <= as.Date("2070-01-01") ~ "2040_2059",
               TRUE ~ "2080_2099",
             )) %>%
      # Reorder the columns to match the standard order
      select(time_period, time, Shortwave, Longwave, AirTemp, RelHum, WindSpeed, Rain, Snow) %>%
      # Group by time period
      group_by(time_period) %>%
      # For each time period, write the data for the current cell to a feather file
      group_map(~ {
        time_period <- .y$time_period
        out_file <- sprintf(outfile_template, gcm_name, time_period, cell_no)
        arrow::write_feather(.x, out_file)
        return(out_file)
      })
  })
  
  # Unnest the list
  time_period_cell_files <- unlist(time_period_cell_files)

  # # tried this code but was too much data for 'reduce' to handle at once
  # # For each variable...
  # all_data <- purrr::map2(nc$data_frames, names(nc$data_frames), function(var_df, var_name) {
  #   # Pivot the data to long format, using the column names (cell_numbers) to define a new column
  #   var_long <- var_df %>%
  #     pivot_longer(colnames(var_df), names_to='data_cell_no', values_to=var_name)
  #   return(var_long) # A two-column-wide dataframe
  # }) %>%
  #   reduce(left_join, by='data_cell_no') %>% #Reduce the list of dataframes to one big dataframe
  #   mutate(time = nc_dates, .before=1) # Add the dates as a column
  
  return(time_period_cell_files)
}


