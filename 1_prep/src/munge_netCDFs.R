
extract_and_write_nc_cell_data <- function(tile_no, lake_cell_tile_xwalk, gcm_name, nc, nc_dates, outfile_template) {
  # For each tile...
  # Pull a vector of cell numbers within that tile
  tile_cell_nos <- lake_cell_tile_xwalk %>%
    filter(data_tile_no == tile_no) %>%
    pull(data_cell_no) %>%
    unique()
  # For each variable stored in the netCDF...
  tile_files <- bind_cols(
    # set up time and data_cell_no columns to match dimensions of long-formatted data
    tidyr::expand_grid(nesting(tibble(time=nc_dates)), data_cell_no=tile_cell_nos), 
    # pull data for all cells in a given tile and pivot to long format
    purrr::map2_dfc(nc$data_frames, names(nc$data_frames), function(var_df, var_name) {
      # Pull the columns pertaining to the vector of cell numbers and pivot to long format
      # Remove data_cell_no column in order to clean up column binding (can't use map2() 
      # and reduce() because of memory limits)
      var_df %>%
        select(as.character(tile_cell_nos)) %>%
        pivot_longer(everything(), names_to='data_cell_no', values_to=var_name) %>%
        select(-data_cell_no)
    })) %>%
    # Once all variables have been compiled for each cell,
    # Determine the time period for each row
    mutate(time_period = case_when(
      time <= as.Date("2010-01-01") ~ "1981_2000",
      time <= as.Date("2070-01-01") ~ "2040_2059",
      TRUE ~ "2080_2099",
    )) %>%
    # Reorder the columns to match the standard order
    select(time_period, time, data_cell_no, Shortwave, Longwave, AirTemp, RelHum, WindSpeed, Rain, Snow) %>%
    # Group by time period
    group_by(time_period) %>%
    # For each time period, write the data for the current tile to a feather file
    group_map(~ {
      time_period <- .y$time_period
      out_file <- sprintf(outfile_template, gcm_name, time_period, tile_no)
      arrow::write_feather(.x, out_file)
      return(out_file)
    })
}

#' @title Split each GCM netCDF into feather files specific to tiles and time periods
#' @decription Create a feather file with driver data for the current gcm, for
#' each tile, for each time period
#' @param gcm_nc filename of a GCM netCDF file
#' @param gcm_name name of one of the six GCMs
#' @param lake_cell_tile_xwalk - mapping of which lakes fall into which gcm cells and tiles
#' (parameters `spatial_cell_no` and `spatial_tile_no`) and which gcm cell to use for driver 
#' data for each lake (`data_cell_no`,`data_tile_no`). `data_cell_no` will only differ from
#' `spatial_cell_no` for those lakes that fall within gcm cells that are missing data.
#' @param outfile_template string representing the filepath at which to save
#' the feather file output of the split netCDF data. The first `%s` is
#' used as the placeholder for the `gcm_name`, the second is for the time period,
#' and the third is for the data_tile_no.
#' @return names of gcm/time-period/tile-specific feather files
munge_nc_files <- function(gcm_nc, gcm_name, lake_cell_tile_xwalk, outfile_template) {
  # Open the netCDF file for the current GCM
  nc <- ncdfgeom::read_timeseries_dsg(gcm_nc)
  
  # Pull the vectors of dates
  nc_dates <- lubridate::as_date(nc$time)

  ### For each tile...
  gcm_tile_nos <- unique(lake_cell_tile_xwalk$data_tile_no)
  # time_period_tile_files <- purrr::map(gcm_tile_nos, extract_and_write_nc_cell_data,
  #                                      lake_cell_tile_xwalk, gcm_name, nc, nc_dates, outfile_template) %>%
  #   unlist()
  time_period_tile_files <- purrr::map(gcm_tile_nos, function(tile_no) {
    # Pull a vector of cell numbers within that tile
    tile_cell_nos <- lake_cell_tile_xwalk %>%
      filter(data_tile_no == tile_no) %>%
      pull(data_cell_no) %>%
      unique()

    # For each variable stored in the netCDF...
    tile_files <- bind_cols(
      # set up time and data_cell_no columns to match dimensions of long-formatted data
      tidyr::expand_grid(nesting(tibble(time=nc_dates)), data_cell_no=tile_cell_nos),
      # pull data for all cells in a given tile and pivot to long format with a single column
      purrr::map2_dfc(nc$data_frames, names(nc$data_frames), function(var_df, var_name) {
        # Pull the columns pertaining to the vector of cell numbers and pivot to long format
        # Remove data_cell_no column in order to clean up column binding (can't use map2()
        # and reduce() because of memory limits)
        var_df %>%
          select(as.character(tile_cell_nos)) %>%
          pivot_longer(everything(), names_to='data_cell_no', values_to=var_name) %>%
          select(-data_cell_no)
      })) %>%
      # Once all variables have been compiled for each cell,
      # Determine the time period for each row
      mutate(time_period = case_when(
        time <= as.Date("2010-01-01") ~ "1981_2000",
        time <= as.Date("2070-01-01") ~ "2040_2059",
        TRUE ~ "2080_2099",
      )) %>%
      # Reorder the columns to match the standard order
      select(time_period, time, data_cell_no, Shortwave, Longwave, AirTemp, RelHum, WindSpeed, Rain, Snow) %>%
      # Group by time period
      group_by(time_period) %>%
      # For each time period, write the data for the current tile to a feather file
      group_map(~ {
        time_period <- .y$time_period
        out_file <- sprintf(outfile_template, gcm_name, time_period, tile_no)
        arrow::write_feather(.x, out_file)
        return(out_file)
      })
  }) %>% unlist()

  return(time_period_tile_files)
}


