#' @title Prep site observations for evaluation
#' @description Prep site observations by reading in the feather file of observations,
#' filtering to sites for which we have model output, and to dates within the model
#' period. Then subset observations to those for sites with >= `min_obs_dates` to 
#' create the set of observations to use in model evaluation
#' @param obs_feather filepath to lake temperature observations file.
#' Once read in, has columns for site_id, date, temp, source
#' @param modeled_sites vector of site_ids for which we have model output
#' @param start_date earliest date for which we want to keep obs
#' @param end_date latest date for which we want to keep obs
#' @param min_obs_dates specified minimum number of days for which a site
#' must have observations to be included in the evaluation set
#' @return A tibble of temperature observations for evaluation sites, with columns for
#' site_id, time, depth, and temperature
get_eval_obs <- function(obs_feather, modeled_sites, start_date, end_date, min_obs_dates) {
  temp_obs <- arrow::read_feather(obs_feather) %>%
    filter(site_id %in% modeled_sites) %>%
    filter(date >= start_date & date <= end_date) %>%
    rename(time = date, temperature = temp) %>% # For consistent naming throughout pipeline
    select(-source)
  
  eval_sites <- temp_obs %>%
    group_by(site_id) %>%
    summarize(n_dates = length(unique(time))) %>% filter(n_dates >= min_obs_dates) %>%
    pull(site_id)
  
  filtered_temp_obs <- temp_obs %>%
    filter(site_id %in% eval_sites) %>%
    arrange(site_id, time, depth)
  
  return(filtered_temp_obs)
}

#' @title Match predictions to observations for a single site
#' @description For a single site, munge model predictions to long format,
#' extrapolate predictions on all dates to the maximum GLM prediction depth or 
#' maximum lake depth (whichever is larger), and then interpolate the predictions  
#' to the depths of the observations. Code adapted from 
#' https://github.com/USGS-R/mntoha-data-release/blob/main/src/eval_utils.R#L14-L35 
#' https://github.com/USGS-R/mntoha-data-release/blob/main/src/eval_utils.R#L113-L132
#' @param preds_file The filepath for temperature predictions to use in evaluation. 
#' The filepath corresponds to predictions for the site_id associated with `eval_obs`
#' @param eval_obs temperature observations for a single site to use in evaluation
#' (in long format).
#' @param lake_depth Tibble of site_id and lake_depth for the current site
#' @param driver weather/climate driver used to generate GLM predictions
#' @return A tibble with predictions matched to observations (on available dates)
#' by date and by depth
match_pred_obs <- function(preds_file, eval_obs, lake_depth, driver) {
  # Read in model predictions for the current site
  # filter to dates with observations
  # and munge model predictions to long format
  eval_preds <- arrow::read_feather(preds_file) %>%
    filter(time %in% eval_obs$time) %>%
    mutate(site_id = unique(eval_obs$site_id)) %>%
    select(-ice) %>%
    munge_long() %>% # Fxn in 4_visualize/src/plot_data_utility_fxns.R
    mutate(depth = as.numeric(depth)) %>% 
    rename(pred = temperature)
  
  # If lake_depth exceeds the maximum GLM prediction depth
  if (lake_depth$lake_depth > max(eval_preds$depth)) {
    # Generate a new dataframe with a row for maximum lake depth for each date
    new_depths <- tibble(
      time = unique(eval_preds$time),
      site_id = lake_depth$site_id,
      depth = lake_depth$lake_depth
    )
    eval_preds <- eval_preds %>%
      bind_rows(new_depths)
  }
  
  # Fill predictions to the maximum GLM prediction depth or to the maximum lake depth,
  # if it exceeds the maximum GLM prediction depth. Fills with the known `pred` right above the new depth
  eval_preds <- eval_preds %>%
    arrange(time, depth) %>%
    fill(pred, .direction=c('down'))
  
  # Rename observed temperature to `obs`
  eval_obs <- eval_obs %>%
    rename(obs = temperature)
  
  # match up preds to test_obs, interpolating predictions to match the observation depths
  pred_obs <- bind_rows(lapply(unique(eval_obs$time), function(selected_time) {
    pred_1d <- filter(eval_preds, time == selected_time, !is.na(depth))
    obs_1d <- filter(eval_obs, time == selected_time)
    interp_1d <- tryCatch({
      if(nrow(pred_1d) == 0) stop(sprintf('no predictions on %s for %s', selected_time, unique(eval_obs$site_id)))
      if(min(pred_1d$depth) != 0) warning(sprintf('no prediction at 0m on %s for %s', selected_time, unique(eval_obs$site_id)))
      mutate(obs_1d, pred = approx(x=pred_1d$depth, y=pred_1d$pred, xout=obs_1d$depth, rule=1)$y)
    }, error=function(e) {
      message(sprintf('approx failed for %s on %s: %s', unique(eval_obs$site_id), selected_time, e$message))
      mutate(obs_1d, pred = NA)
    })
    return(interp_1d)
  })) %>%
    filter(!is.na(pred)) %>% # Filter out rows where there are no matching predictions for observed depths (e.g. observed depth exceeds lake depth)
    mutate(driver = driver, .after=site_id) %>%
    select(-tar_group) # drop tar_group field
  
  return(pred_obs)
}

#' @title prep pred-obs data for evaluation
#' @description add grouping variables to matched pred-obs for evaluation.
#' @param pred_obs the tibble of matched observations and predictions
#' @param lake_depths tibble of lake depths for evaluation sites
#' @param surface_max_depth maximum depth for which predictions are
#' considered to be in the 'surface' depth class. Currently this is set
#' as a global value, and is not lake-specific
#' @param bottom_depth_factor a factor indicating the proportion
#' of the water column considered to be *outside* of the 'bottom' depth 
#' class. When multiplied by each lake's depth, the resulting value is the 
#' minimum depth for which predictions for each lake are considered to be 
#' in the 'bottom' depth class
#' @param doy_bin_size # of days to include in each doy bin
#' @param temp_bin_size # of degrees to include in each temperature bin
#' @return a tibble of matched predictions and observations with the 
#' difference between the prediction and observation (pred_diff), the
#' depth_class (surface, middle, or bottom), year, doy, doy_bin (size 
#' set by `doy_bin_size`), and season of each pred-obs pair, and the 
#' temperature bin (size set by `temp_bin_size`) into which each observation falls 
prep_data_for_eval <- function(pred_obs, lake_depths, surface_max_depth, bottom_depth_factor, doy_bin_size, temp_bin_size) {
  
  eval_pred_obs <- pred_obs %>%
    left_join(lake_depths, by='site_id') %>%
    mutate(pred_diff = pred - obs,
           depth_class = case_when(
             depth <= surface_max_depth ~ 'surface',
             depth >= (bottom_depth_factor * lake_depth) ~ 'bottom',
             TRUE ~ 'middle'
           ),
           depth_class = factor(depth_class,levels=c('surface','middle','bottom')),
           year = year(time),
           doy = yday(time),
           doy_bin = doy_bin_size*ceiling(doy/doy_bin_size),
           season = case_when(
             doy >= 60 & doy < 173 ~ 'spring',
             doy >= 173 & doy < 243 ~ 'summer',
             doy >= 243 & doy < 335 ~ 'fall',
             TRUE ~ 'winter'
           ),
           season = factor(season, levels=c('winter','spring','summer','fall')),
           temp_bin = temp_bin_size*ceiling(obs/temp_bin_size))
  
  return(eval_pred_obs)
}

#' @title Calculate bias
#' @description Calculate the bias of model predictions over a 
#' specified `grouping_var`
#' @param eval_pred_obs a tibble of the matched model predictions and 
#' observations, along with grouping variables for evaluation
#' @param grouping_var the variable by which to group `eval_pred_obs`
#' before calculating the bias
#' @param driver weather/climate driver used to generate GLM predictions
#' @param depth_class the depth bin for the matched `eval_pred_obs`
#' @return a tibble grouped and summarized by the grouping_var, with 
#' a row for each group and columns for the bias, # of dates, # of sites, 
#' driver, and depth class
calc_bias <- function(eval_pred_obs, grouping_var, driver, depth_class) {
  # confirm that only matched pred-obs for a single driver were provided
  stopifnot(length(driver) == 1)
  # confirm that only matched pred-obs for a single depth class were provided
  stopifnot(length(depth_class) == 1)
  
  # calculate bias
  eval_pred_obs %>%
    group_by(!!sym(grouping_var)) %>%
    summarize(bias = median(pred_diff, na.rm=TRUE),
              n_dates = n(),
              n_sites = length(unique(site_id))) %>%
    mutate(driver = driver, depth_class = depth_class, .before=1)
}

#' @title Calculate rmse
#' @description Calculate the rmse of model predictions over a 
#' specified `grouping_var`
#' @param eval_pred_obs a tibble of the matched model predictions and 
#' observations, along with grouping variables for evaluation
#' @param grouping_var the variable by which to group `eval_pred_obs`
#' before calculating the rmse
#' @param driver weather/climate driver used to generate GLM predictions
#' @return a tibble grouped and summarized by the grouping_var, with 
#' a row for each group and columns for the rmse, # of dates, # of sites, 
#' driver, and depth class
#' @param depth_class the depth bin for the matched `eval_pred_obs`
calc_rmse <- function(eval_pred_obs, grouping_var, driver, depth_class) {
  # confirm that only matched pred-obs for a single driver were provided
  stopifnot(length(driver) == 1)
  # confirm that only matched pred-obs for a single depth class were provided
  stopifnot(length(depth_class) == 1)
  
  # calculate rmse
  eval_pred_obs %>%
    group_by(!!sym(grouping_var)) %>%
    summarize(rmse = sqrt(mean((pred_diff)^2, na.rm=TRUE)),
              n_dates = n(),
              n_sites = length(unique(site_id))) %>%
  mutate(driver = driver, depth_class = depth_class, .before=1)
}

#' @title Plot evaluation metrics as a bar plot
#' @description generate a barplot of an evaluation metric, using the
#' specified x and y variables
#' @param plot_df a tibble of the matched model predictions and observations,
#' along with grouping variables for evaluation
#' @param num_eval_sites The number of unique evaluation sites 
#' @param driver_type the type of driver used to generate the model
#' predictions (NLDAS or GCM)
#' @param y_var the variable for the y-axis of the plot
#' @param y_label the label for the y-axis of the plot
#' @param x_var the variable for the x-axis of the plot
#' @facet_column_variable variable to use for faceting the columns of the plot
#' @facet_row_variable variable to use for faceting the rows of the plot
#' @param outfile The filepath for the exported png
#' @return The filepath of the exported png 
plot_evaluation_barplot <- function(plot_df, num_eval_sites, driver_type, y_var, y_label, x_var, facet_column_variable, 
                                    facet_row_variable, outfile, plot_dpi = 300, plot_width = 10, plot_height = 8) {
  bar_plot <- plot_df %>%
    ggplot(aes(x = get(x_var), y = get(y_var))) +
    geom_col(fill='cadetblue3', color='cadetblue4') +
    labs(title = paste(sprintf("%s predictions: %s by %s", driver_type, y_var, x_var),
                       sprintf("Total # of evaluation sites: %s", num_eval_sites), sep ='\n'), 
         x=sprintf("%s", x_var), 
         y=sprintf("%s (\u00b0C)", y_label)) +
    facet_grid(get(facet_row_variable) ~ get(facet_column_variable)) +
    theme_bw()
  
  ggsave(filename=outfile, plot=bar_plot, dpi=plot_dpi, width=plot_width, height=plot_height)
  return(outfile)
}