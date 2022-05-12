#' @title Assign the correct time period string to the current date
#' @description Take a date and return the appropriate time period
#' string. Currently assumes that the time periods are one of 
#' "1981_2000", "2040_2059", or "2080_2099". Could be improved later
#' to handle others.
#' @param time vector of `Date` class values
#' @param gcm_time_periods vector of the character strings representing 
#' available time periods for the data. One of "1981_2000", "2040_2059", 
#' or "2080_2099"
#' @return vector of the same length as `time` with the time period strings
date_to_gcm_time_period <- function(time, gcm_time_periods) {
  dplyr::case_when(
    time <= as.Date("2010-01-01") ~ gcm_time_periods[1],
    time <= as.Date("2070-01-01") ~ gcm_time_periods[2],
    TRUE ~ gcm_time_periods[3]
  )
}

#' @title Add burn-in and burn-out to raw meteo data
#' @description function to add burn-in and burn-out to the raw meteo
#' data by mirroring the data. From Jordan's GLM projection code: 
#' https://github.com/jread-usgs/lake-temperature-process-models/blob/master/3_run/src/run_glm_utils.R#L62-L76
#' @param meteo_data the raw meteo data
#' @param burn_in length of burn-in period, in days
#' @param burn_out length of burn-out period, in days
#' @return a munged dataframe of meteorological data that includes
#' burn-in and burn-out time
add_burn_in_out_to_meteo <- function(meteo_data, burn_in = 0, burn_out = 0){

  # add the burn-in/burn-out 
  # burn-in length and burn-out length have already been checked against
  # the length of the raw meteo data in `munge_gcm_dates()`
  if (burn_in > 0) {
    # create a burn-in. We're going to mirror the data of the first year:
    burn_in_data <- meteo_data[2:(burn_in+1), ] %>% arrange(desc(time)) %>%
      mutate(time = seq(from = meteo_data[1,]$time-burn_in, by = 'day', length.out = length(2:(burn_in+1))))
    # bind with real data, prior to the begin date. We're not duplicating dates, hence the "2"
    meteo_data <- bind_rows(burn_in_data, meteo_data)
  }
  if (burn_out > 0) {
    # create a burn-out. Mirror the final data:
    burn_out_data <- tail(meteo_data, (burn_out+1)) %>% head(-1L) %>% arrange(desc(time)) %>%
      mutate(time = seq(from = tail(meteo_data,1)$time+1, by = 'day', length.out = length(2:(burn_out+1))))
    # bind with real data, at the end of the sequence
    meteo_data <- bind_rows(meteo_data, burn_out_data)
  }
  
  return(meteo_data)
}

#' @title Split each GCM netCDF into csv files specific to cells and time periods
#' @decription Create a csv file with driver data for the current gcm, for
#' each cell, for each time period
#' @param gcm_nc filename of a GCM netCDF file
#' @param gcm_name name of one of the six GCMs
#' @param cell_nos vector of GCM cell ids
#' @param gcm_dates - a tibble with a row for each time period, and columns specifying
#' the start and end date of each time period, as well as the length of
#' burn in, the burn-in start date, the length of burn-out, and the burn-out
#' end date for each time period
#' @param outfile_template string representing the filepath at which to save
#' the csv file output of the split netCDF data. The first `%s` is
#' used as the placeholder for the `gcm_name`, the second is for the time period,
#' and the third is for the data_cell_no
#' @return names of gcm/time-period/cell-specific csv files
munge_gcm_nc_files <- function(gcm_nc, gcm_name, cell_nos, gcm_dates, outfile_template) {
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
             time_period = date_to_gcm_time_period(time, gcm_dates$time_period)
             ) %>%
      # Reorder the columns to match the standard order
      select(time_period, time, Shortwave, Longwave, AirTemp, RelHum, WindSpeed, Rain, Snow) %>%
      # Group by time period
      group_by(time_period) %>%
      # For each time period...
      group_map(~ {
        group_time_period <- .y$time_period
        time_period_date_info <- filter(gcm_dates, time_period==group_time_period)
        # add the specified number of burn_in and burn_out days
        meteo_w_burn_in_out <- add_burn_in_out_to_meteo(.x, time_period_date_info$burn_in, time_period_date_info$burn_out)
        # then write the data for the current cell to a csv file
        outfile <- sprintf(outfile_template, gcm_name, group_time_period, cell_no)
        fwrite(meteo_w_burn_in_out, outfile)
        return(outfile)
      })
  }) %>% unlist()

  return(time_period_cell_files)
}

#' @title check length of requested burn-in/out
#' @description for a given time period, check whether the length of the 
#' requested burn-in/out exceeds the length of the raw meteorological 
#' data, and, if so, note that no burn-in/out will be added
#' @param time_period - the model time period, defined by
#' bracketing years
#' @param driver_start_date - the first date in the time period
#' @param driver_end_date - the final date in the time period
#' @param driver_type - 'gcm' or 'nldas'
#' @param burn_type - burn-in / burn-out depending on which is being checked
#' @param burn_days - the length of the requested burn-in/out, in days
#' @return the # of burn-in/out days that will be added to the meteorological
#' data for the time period
check_burn_length <- function(time_period, driver_start_date, driver_end_date, driver_type, burn_type, burn_days) {
  # compute length of raw gcm data, in days
  ndays_meteo <- driver_end_date-driver_start_date
  
  # Check length of requested burn-in/out against length of raw gcm data
  if (burn_days > (ndays_meteo)) {
    message(paste(sprintf('Checking requested %s for %s %s data', burn_type, time_period, driver_type),
                  sprintf('The requested %s length (%s days) exceeds', burn_type, burn_days),
                  sprintf('the length of the raw meteorological data (%s days).', ndays_meteo),
                  sprintf('No %s will be added to the %s data for the time period %s.', burn_type, driver_type, time_period), 
                  '', sep = "\n"))
    burn_days <- 0
  }

  return(burn_days)
}

#' @title Munge GCM dates
#' @description Function to determine the start and end dates of each
#' GCM time period and document the date when burn-in starts and the date
#' when burn-out ends, checking that the requested burn-in/out doesn't
#' exceed the length of the raw GCM meteorological data.
#' @param gcm_ncs filenames of the GCM netCDF files
#' @param gcm_time_periods - the three GCM time periods, defined by their 
#' bracketing years
#' @param burn_in length of the requested burn-in period, in days. Used 
#' to mirror the start of the raw meteorological data to create a burn-in 
#' period for the model simulation.
#' @param burn_out length of the requested burn-out period, in days. Used 
#' to mirror the end of the raw meteorological data to create a burn-out 
#' period for the model simulation.
#' @return a tibble with a row for each time period, and columns specifying
#' the start and end date of each time period, as well as the length of
#' burn in, the burn-in start date, the length of burn-out, and the burn-out
#' end date for each time period
munge_gcm_dates <- function(gcm_ncs, gcm_time_periods, burn_in, burn_out) {
  # Use first nc file, since all have the same date range
  nc <- ncdfgeom::read_timeseries_dsg(gcm_ncs[1])
  
  nc_dates <- tibble(
    time = lubridate::as_date(nc$time),
    time_period = date_to_gcm_time_period(time, gcm_time_periods)
    ) %>%
    group_by(time_period) %>%
    summarize(driver_start_date = min(time), driver_end_date = max(time)) %>%
    mutate(
      driver_type = 'gcm',
      # a burn-in period only will have been added if the requested burn-in period was 
      # shorter than the length of the raw meteo data
      burn_in = check_burn_length(time_period, driver_start_date, driver_end_date, driver_type = 'gcm', 'burn-in', burn_in),
      burn_in_start = driver_start_date-burn_in,
      # a burn-out period only will have been added if the requested burn-out period was 
      # shorter than the length of the raw meteo data
      burn_out = check_burn_length(time_period, driver_start_date, driver_end_date, driver_type = 'gcm', 'burn-out', burn_out),
      burn_out_end = driver_end_date+burn_out) %>%
    relocate(driver_type, .before=time_period)
  
  return(nc_dates)
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
      driver_type = 'nldas',
      driver_start_date = as.Date(sprintf('%s-01-01', model_years[1])), # set based on user-defined modeling period
      driver_end_date = as.Date(sprintf('%s-12-31', model_years[2])), # set based on user-defined modeling period
      # the burn-in period is the period that precedes the first full year of the NLDAS time period
      burn_in = driver_start_date - burn_in_start,
      # the burn-out period is the period that follows the final full year of the NLDAS time period
      burn_out = burn_out_end - driver_end_date) %>%
    relocate(driver_type, .before=time_period)
  
  return(nldas_dates)
}
