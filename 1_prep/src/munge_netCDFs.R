#' @title Add burn-in and burn-out to raw meteo data for each cell
#' @description function to add burn-in and burn-out to the raw meteo
#' data by mirroring the data. From Jordan's GLM projection code: 
#' https://github.com/jread-usgs/lake-temperature-process-models/blob/master/3_run/src/run_glm_utils.R#L62-L76
#' @param meteo_data the raw meteo data for the cell, read in from 
#' the netCDF file
#' @param burn_in length of burn-in period, in days
#' @param burn_out length of burn-out period, in days
#' @return a munged dataframe of meteorological data that includes
#' burn-in and burn-out time
add_burn_in_out_to_meteo <- function(meteo_data, burn_in = 300, burn_out = 190){

  # add the burn-in/burn-out if the requested burn-in/burn-out period is 
  # shorter than the length of the raw meteo data
  ndays_meteo <- nrow(meteo_data) #data is formatted as one row per day
  if (burn_in <= ndays_meteo) {
    # create a burn-in. We're going to mirror the data of the first year:
    burn_in_data <- meteo_data[2:burn_in, ] %>% arrange(desc(time)) %>%
      mutate(time = seq(from = meteo_data[1,]$time-burn_in+1, by = 'day', length.out = length(2:burn_in)))
    # bind with real data, prior to the begin date. We're not duplicating dates, hence the "2"
    meteo_data <- bind_rows(burn_in_data, meteo_data)
  } else {
    message(paste(sprintf('The requested burn-in length (%s days) exceeds', burn_in),
                  sprintf('the length of the raw meteorological data (%s days).', ndays_meteo),
                  'No burn-in will be added.',
                  sep = "\n"))
  }
  if (burn_out <= ndays_meteo) {
    # create a burn-out. Mirror the final data:
    burn_out_data <- tail(meteo_data, burn_out) %>% head(-1L) %>% arrange(desc(time)) %>%
      mutate(time = seq(from = tail(meteo_data,1)$time+1, by = 'day', length.out = length(2:burn_out)))
    # bind with real data, at the end of the sequence
    meteo_data <- bind_rows(meteo_data, burn_out_data)
  } else {
    message(paste(sprintf('The requested burn-out length (%s days) exceeds', burn_out),
                  sprintf('the length of the raw meteorological data (%s days).', ndays_meteo),
                  'No burn-out will be added.',
                  sep = "\n"))
  }
  return(meteo_data)
}

#' @title Split each GCM netCDF into csv files specific to tiles and time periods
#' @decription Create a csv file with driver data for the current gcm, for
#' each cell, for each time period
#' @param gcm_nc filename of a GCM netCDF file
#' @param gcm_name name of one of the six GCMs
#' @param cell_nos vector of GCM cell ids
#' @param gcm_time_periods - the three GCM time periods, defined by their bracketing years
#' @param burn_in length of burn-in period, in days. Used to mirror the 
#' start of the raw meteorological data to create a burn-in period for 
#' the model simulation.
#' @param burn_out length of burn-out period, in days. Used to mirror the 
#' end of the raw meteorological data to create a burn-out period for 
#' the model simulation.
#' @param outfile_template string representing the filepath at which to save
#' the csv file output of the split netCDF data. The first `%s` is
#' used as the placeholder for the `gcm_name`, the second is for the time period,
#' and the third is for the data_cell_no
#' @return names of gcm/time-period/cell-specific csv files
munge_nc_files <- function(gcm_nc, gcm_name, cell_nos, gcm_time_periods, burn_in, burn_out, outfile_template) {
  # Open the netCDF file for the current GCM
  nc <- ncdfgeom::read_timeseries_dsg(gcm_nc)
  
  # Pull the vectors of dates
  nc_dates <- lubridate::as_date(nc$time)
  
  # Generate a list of time-period specific and cell-specific files
  # For each cell...
  time_period_cell_files <- purrr::map(cell_nos, function(cell_no) {
    # For each variable stored in the netCDF...
    cell_files <- purrr::map2_dfc(nc$data_frames, names(nc$data_frames), function(var_df, var_name) {
      # Pull the column pertaining to the current cell and rename it to the variable name
      var_cell_long <- var_df %>%
        select(as.character(cell_no)) %>%
        rename(!!var_name := as.character(cell_no))
    }) %>%
      # Once all variables have been compiled for each cell,
      # Add a column for time and determine the time period for each row
      mutate(time = nc_dates,
             time_period = case_when(
               time <= as.Date("2010-01-01") ~ gcm_time_periods[1],
               time <= as.Date("2070-01-01") ~ gcm_time_periods[2],
               TRUE ~ gcm_time_periods[3])
             ) %>%
      # Reorder the columns to match the standard order
      select(time_period, time, Shortwave, Longwave, AirTemp, RelHum, WindSpeed, Rain, Snow) %>%
      # Group by time period
      group_by(time_period) %>%
      # For each time period...
      group_map(~ {
        time_period <- .y$time_period
        # add the specified number of burn_in and burn_out days
        meteo_w_burn_in_out <- add_burn_in_out_to_meteo(.x)
        # then write the data for the current cell to a csv file
        outfile <- sprintf(outfile_template, gcm_name, time_period, cell_no)
        fwrite(setDT(meteo_w_burn_in_out), outfile)
        return(outfile)
      })
  }) %>% unlist()

  return(time_period_cell_files)
}

#' @title Munge GCM dates
#' @description Function to determine the start and end dates of each
#' GCM time period and document the date when burn-in stars and the date
#' when burn-out ends
#' @param gcm_nc filenames of the GCM netCDF files
#' @param gcm_time_periods - the three GCM time periods, defined by their 
#' bracketing years
#' @param burn_in length of burn-in period, in days. Used to mirror the 
#' start of the raw meteorological data to create a burn-in period for 
#' the model simulation.
#' @param burn_out length of burn-out period, in days. Used to mirror the 
#' end of the raw meteorological data to create a burn-out period for 
#' the model simulation.
#' @return a tibble with a row for each time period, and columns specifying
#' the start and end date of each time period, as well as the length of
#' burn in, the burn-in start date, the length of burn-out, and the burn-out
#' end date for each time period
munge_gcm_dates <- function(gcm_time_periods, gcm_ncs, burn_in, burn_out) {
  # Use first nc file, since all have the same date range
  nc <- ncdfgeom::read_timeseries_dsg(gcm_ncs[1])
  
  nc_dates <- tibble(
    time = lubridate::as_date(nc$time),
    time_period = case_when(
      time <= as.Date("2010-01-01") ~ gcm_time_periods[1],
      time <= as.Date("2070-01-01") ~ gcm_time_periods[2],
      TRUE ~ gcm_time_periods[3])
    ) %>%
    group_by(time_period) %>%
    summarize(gcm_start_date = min(time), gcm_end_date = max(time)) %>%
    mutate(burn_in = burn_in,
           burn_in_start = gcm_start_date-burn_in+1,
           burn_out = burn_out,
           burn_out_end = gcm_end_date+burn_out-1)
  
  return(nc_dates)
}

