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
  
  filtered_temp_obs <- filter(temp_obs, site_id %in% eval_sites)
  
  return(filtered_temp_obs)
}

#' @title Match predictions to observations
#' @description Interpolate the model predictions to the depths of the observations
#' @param eval_obs temperature observations to use in evaluation, grouped by site.
#' The function maps over these groups.
#' @param eval_preds temperature predictions to use in evaluation, grouped by site.
#' The function maps over these groups.
#' @return A tibble with predictions matched to observations (on available dates)
#' by date and by depth
match_pred_obs <- function(eval_obs, eval_preds) {
  # Filter model predictions to those on dates with obercations
  eval_preds <- eval_preds %>%
    mutate(depth = as.numeric(depth)) %>% 
    filter(time %in% eval_obs$time) %>%
    rename(pred = temperature)
  
  eval_obs <- eval_obs %>%
    rename(obs = temperature)
  
  # match up preds to test_obs, interpolating predictions to match the observation depths
  pred_obs <- bind_rows(lapply(unique(eval_obs$time), function(selected_time) {
    pred_1d <- filter(eval_preds, time == selected_time, !is.na(depth))
    obs_1d <- filter(eval_obs, time == selected_time)
    interp_1d <- tryCatch({
      if(nrow(pred_1d) == 0) stop(sprintf('no predictions on %s', selected_time))
      if(min(pred_1d$depth) != 0) warning(sprintf('no prediction at 0m on %s', selected_time))
      mutate(obs_1d, pred = approx(x=pred_1d$depth, y=pred_1d$pred, xout=obs_1d$depth, rule=1)$y)
    }, error=function(e) {
      message(sprintf('approx failed on %s: %s', selected_time, e$message))
      mutate(obs_1d, pred = NA)
    })
    return(interp_1d)
  }))
  
  return(pred_obs)
}

#' @title prep pred-obs data for evaluation
#' @description filter pred-obs data to selected_depth and add grouping
#' variables for evaluation.
#' @param pred_obs the tibble of matched observations and predictions
#' @param selected_depth depth to which to filter the pred-obs data
#' @return a tibble of matched predictions and observations at the
#' selected depth with the difference between the prediction and observation
#' (pred_diff), the year, doy, month, and season of each pred-obs pair,
#' and the temperature bin into which each observation falls 
prep_data_for_eval <- function(pred_obs, selected_depth) {
  eval_pred_obs <- pred_obs %>%
    filter(depth==selected_depth) %>%
    mutate(pred_diff = pred - obs,
           year = year(time),
           doy = yday(time),
           month = month(time),
           season = case_when(
             month >= 3 & month <= 5 ~ 'spring',
             month >= 6 & month <= 8 ~ 'summer',
             month >= 9 & month <= 11 ~ 'fall',
             TRUE ~ 'winter'
           ),
           temp_bin = 2*ceiling(obs/2))
  
  return(eval_pred_obs)
}

#' @title Calcuate bias
#' @description Calculate the bias of model predictions over a 
#' specified `grouping_var`
#' @param eval_pred_obs a tibble of the matched model predictions and 
#' observations, along with grouping variables for evaluation
#' @param grouping_var the variable by which to group `pred-obs`
#' before calculating the bias
#' @return
calc_bias <- function(eval_pred_obs, grouping_var) {
  eval_pred_obs %>%
    group_by(!!sym(grouping_var)) %>%
    summarize(bias = median(pred_diff))
}

#' @title Calculate rmse
#' @description Calculate the rmse of model predictions over a 
#' specified `grouping_var`
#' @param eval_pred_obs a tibble of the matched model predictions and 
#' observations, along with grouping variables for evaluation
#' @param grouping_var the variable by which to group `pred-obs`
#' before calculating the rmse
#' @return a tibble grouped by the grouping_var, with a column
#' for rmse
calc_rmse <- function(eval_pred_obs, grouping_var) {
  eval_pred_obs %>%
    group_by(!!sym(grouping_var)) %>%
    summarize(rmse = sqrt(mean((pred_diff)^2, na.rm=TRUE)))
}

#' @title Plot evaluation metrics as a bar plot
#' @description generate a barplot of an evaluation metric, using the
#' specified x and y variables
#' @param plot_df a tibble of the matched model predictions and observations,
#' along with grouping variables for evaluation
#' @param driver the name of the driver used to generate the
#' model predictions
#' @param y_var the variable for the y-axis of the plot
#' @param x_var the variable for the x-axis of the plot
#' @param depth depth of the plotted pred-obs
#' @param outfile The filepath for the exported png
#' @return The filepath of the exported png 
plot_evaluation_barplot <- function(plot_df, driver, y_var, x_var, depth, outfile) {
  bar_plot <- plot_df %>%
    ggplot(aes(x = get(x_var), y = get(y_var))) +
    geom_col(fill='cadetblue3', color='cadetblue4') +
    labs(title= sprintf("%s predictions: %s by %s", driver, y_var, x_var), 
         x=sprintf("%s", x_var), 
         y=sprintf("%s (\u00b0C)", y_var)) +
    theme_bw()
  
  ggsave(filename=outfile, plot=bar_plot, dpi=300, width=10, height=6)
  return(outfile)
}