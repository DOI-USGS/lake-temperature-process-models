#' @title Get site-specifc GLM predictions
#' @description function to pull and combine GLM predictions for a single site
#' for all 3 simulation periods based on the input meteo file and export file 
#' vectors provided - may be pulling predictions for a single lake-gcm combo, 
#' or predictions for all GCMS for a single lake.
#' @param site_id The id for the current lake
#' @param site_meteo_files the filepaths for the raw meteo data. The meteo data
#' is used to determine the correct begin and end dates for the predictions and
#' exclude predictions for any burn-in/burn-out periods.
#' @param export_files the filepaths for the GLM predictions for the current lake
#' for each time period and each GCM, if predictions for multiple GCMs are being 
#' read in.
#' @param run_gcm_list A list of GCM names that corresponds to the `site_meteo_files` 
#' and `export_files` that are being read in. Depending on whether predictions are being
#' read in for a single lake-gcm combo, or for all GCMs for a single lake, it may 
#' be a list of the same GCM name repeated, or a list that includes all 6 GCM names.
#' @return A munged dataframe of GLM predictions that includes columns for date, month, 
#' year, DOY, decade, period, gcm, ice, hice (ice thickness), evap (evaporation),
#' n_layers (number of simulated layers), and temperature predictions at 0.5-meter intervals
get_site_preds <- function(site_id, site_meteo_files, site_export_files, run_gcm_list) {
  site_preds <- purrr::pmap_df(list(site_meteo_files, site_export_files, run_gcm_list), function(raw_meteo_fl, export_file, gcm) {
    # Define time period begin and end dates from raw meteo_fl
    meteo_data <- arrow::read_feather(raw_meteo_fl, col_select = "time")
    begin <- min(meteo_data$time)
    end <- max(meteo_data$time)
    # read in data for that time period and truncate
    arrow::read_feather(export_file) %>%
      filter(time >= as.Date(begin) & time <= as.Date(end)) %>%
      mutate(gcm = gcm)
  }) %>%
    mutate(site_id = site_id, date = as.Date(time), .before=1) %>%
    select(-time) %>%
    mutate("month" = month(date),
           "year"= year(date), 
           "doy"= yday(date),
           "decade" = sprintf('%ss', year - (year %% 10)),
           "period" = sprintf('%s - %s', year - (year %% 20), (year - (year %% 20)) + 20),
           .after=date) %>%
    filter(!(year == 2000)) %>% #Predictions for this year are incomplete and fall in a different 20-year period, so exclude
    select(site_id, date, month, year, doy, decade, period, gcm, ice, hice, evap, n_layers, everything()) # Place the temperature prediction columns last
  
  return(site_preds)
}

#' @title Get surface, middle, and bottom GLM predictions for all lakes
#' @description Function to pull and combine GLM predictions for all lakes,
#' across all 3 simulation periods and all 6 GCMs
#' @param run_groups The `p2_glm_uncalibrated_run_groups` grouped version 
#' of the `p2_glm_uncalibrated_runs` output tibble subset to the site_id, 
#' gcm, time_period, raw_meteo_fl, export_fl, and export_fl_hash columns 
#' and grouped by site_id and gcm. Then filtered to only groups for which 
#' glm_success==TRUE for all runs in that group.
#' @return A munged dataframe of surface, middle, and bottom temperature GLM
#' predictions for all successfully simulated lakes. The munge table includes 
#' the columns site_id, gcm, date, month, year DOY, decade, period, gcm, 
#' ice, hice (ice thickness), evap (evaporation), n_layers (number of simulated 
#' layers), depth_class (surface, middle, or bottom), depth, and temperature 
get_surface_middle_bottom_preds <- function(run_groups) {
  surface_middle_bottom_preds <- run_groups %>%
    group_by(site_id) %>%
    group_modify(~ {
      site_meteo_files <- .x$raw_meteo_fl
      site_export_files <- .x$export_fl
      run_gcm_list <- .x$gcm
      # Pull predictions for each site
      site_preds <- get_site_preds(site_meteo_files, site_export_files, run_gcm_list)
      
      # Munge to long format
      site_preds_long <- munge_long(site_preds)
      
      # Identify surface, middle, and bottom depths for each site and filter
      # predictions to only those depths
      plot_depths <- get_plotting_depths(site_preds_long)
      filtered_preds <- filter(site_preds_long, depth %in% plot_depths) %>%
        mutate(depth_class=case_when(
          depth==plot_depths$plot_depth_surface ~ 'surface',
          depth==plot_depths$plot_depth_middle ~ 'middle',
          depth==plot_depths$plot_depth_bottom ~ 'bottom'
        ), .before=depth)
      return(filtered_preds)
    })
  return(surface_middle_bottom_preds)
}

#' @title Munge GLM predictions from wide to long format
#' @description pivots wide-format GLM predictions (with columns like
#' 'temp_0', 'temp_0.5', etc.) to long format, with a column for 'depth'
#' and a column for 'temperature'
#' @param input_wide The wide-formatted input data
#' @return a munged dataframe of GLM predictions that includes all of 
#' the columns in the original wide data except the 'temp_X' columns,
#' which are instead reduced to two columns: 'depth' and 'temperature'
munge_long <- function(input_wide) {
  input_long <- input_wide %>%
    pivot_longer(starts_with("temp_"), names_to="depth", values_to="temperature") %>%
    mutate(depth = as.numeric(str_remove(depth, 'temp_'))) %>%
    arrange(date, depth)
}

#' @title Get the surface, middle, and bottom depths for a given lake, 
#' for plotting. Modified from Alison's code for picking surface, middle, 
#' and bottom depths from observational data: 
#' https://code.usgs.gov/wma/wp/lake-temperature-neural-networks/-/blob/master/3_assess/src/inspect_site.R#L81-93
#' @description For a given lake, identify which depths represent the
#' surface, middle, and bottom sections of the lake
#' @param predictions_long A long-format dataset of GLM predictions at
#' 0.5-meter intervals
#' @return A named list of the surface, middle, and bottom depths to be used
#' to plot data for a given lake.
get_plotting_depths <- function(predictions_long) {
  depth_scores <- predictions_long %>% group_by(depth) %>% tally() %>%
    mutate(
      n_score = n/max(n),
      surface_score = n_score - abs(depth - 0),
      middle_score = n_score - abs(depth - max(depth)/3),
      bottom_score = n_score - abs(depth - max(depth))
    )
  depths <- c(
    surface = depth_scores$depth[which.max(depth_scores$surface_score)],
    middle = depth_scores$depth[which.max(depth_scores$middle_score)],
    bottom = depth_scores$depth[which.max(depth_scores$bottom_score)])
  
  plot_depth_surface = unname(depths['surface'])
  plot_depth_middle = unname(depths['middle'])
  plot_depth_bottom = unname(depths['bottom'])-0.5 # reduce by 0.5m to try to avoid NA values, which can occur at the bottom
  return(list(plot_depth_surface=plot_depth_surface, plot_depth_middle=plot_depth_middle, plot_depth_bottom=plot_depth_bottom))
}