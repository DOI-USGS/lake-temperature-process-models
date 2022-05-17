#' @title Create a violin chart of predicted surface, middle, and bottom 
#' temperatures in all lakes for all GCMs
#' @description Plot the distribution of predicted temperatures
#' across all lakes, broken out by depth class (surface, middle or bottom),
#' by time period. The output plot is faceted either by driver (gcm name) or by month
#' (only even months), as set by `faceting_variable`, and by time period
#' @param glm_preds Long-formatted temperature predictions for every
#' lake-gcm combo for which model runs for all 3 time periods succeeded,
#' filtered to only surface, middle, and bottom depths for each lake
#' @param faceting_variable variable that will set the columns of the faceted 
#' plot, either 'driver' or 'month'. The rows for all plots will be time periods.
#' @param outfile The filepath for the exported png
#' @return The filepath of the exported png 
plot_temp_violin <- function(glm_preds, faceting_variable, outfile) {
  
  # If faceting by months...
  if (faceting_variable == 'month') {
    # filter GLM preds to only even months
    glm_preds <- glm_preds %>%
      filter(month %% 2 == 0)
  }
  
  # Build plot
  plot_title <- paste(sprintf('Distribution of temperatures across depth classes, by %s and time period', 
                              ifelse(faceting_variable=='month', paste0('(even) ', faceting_variable, 's'), faceting_variable)), 
                        sprintf('number of lakes: %s', length(unique(glm_preds$site_id))), sep='\n')
  violin_plot <- ggplot(glm_preds) + 
    geom_violin(aes(x=depth_class, y=temperature,color=depth_class), trim=FALSE) + 
    facet_grid(period~get(faceting_variable)) +
    labs(title = plot_title) +
    theme(axis.title.x=element_blank())
  
  # Save plot
  ggsave(filename=outfile, plot=violin_plot, dpi=300, width=15, height=6)
  return(outfile)
}

#' @title Plot predicted surface, middle, and bottom temperatures in a 
#' given lake, for a single GCM or for all 6 GCMs
#' @description For a single lake, all 3 time periods, and 1 or 6 GCMs,
#' plot surface, middle, and bottom temperature predictions in each year, 
#' by DOY, as well as the average GLM prediction for each DOY in each 
#' 20-year time period. Also plot whether or not the model predicted ice 
#' on a given DOY for any GCM (or, if plotting all GCMS, for all 6 GCMs). 
#' The output plot is faceted by time period and by depth class
#' @param glm_preds_long Long-formatted temperature predictions for the
#' current lake, for a single GCM or for all GCMs (6 values for each depth 
#' on each calendar date) if `all_gcms` is TRUE
#' @param glm_mean_long Long-formatted mean temperature predictions for
#' each DOY across all years, for a single GCM or for all GCMs (single 
#' value for each depth on each doy in each time period) if `all_GCMs` is
#' TRUE
#' @param all_gcms logical parameter indicating whether or not the plot
#' includes data for all 6 GCMs
#' @param outfile_template The template filepath for the exported png, which
#' is customized with the site_id
#' @return The filepath of the exported png 
plot_20yr_preds_ice <- function(glm_preds_long, glm_mean_long, all_gcms, outfile_template) {
  
  # Pull out ice predictions, and create offset temperature for plotting
  glm_preds_ice <- glm_preds_long %>%
    select(date, year, doy, period, driver, ice, depth, temperature) %>%
    filter(depth == 0) %>%
    mutate(temperature = -1) %>% # could use actual surface temp, but artificially set to -1 so doesn't block surface temp pred
    filter(ice ==TRUE) # filter to only days when ice was present in any year/for any gcm
  
  # Get dates when all years (or all years and all GCMs) predicted ice
  glm_preds_ice_mean <- glm_preds_long %>%
    select(date, year, doy, period, driver, ice, depth, temperature) %>%
    filter(depth == 0) %>%
    group_by(depth, doy, period) %>%
    summarize(ice_all = ifelse(sum(ice)==n(), TRUE, FALSE), mean_temp = -2) %>% # could use actual surface temp, but artificially set to -2
    filter(ice_all == TRUE) # filter to only days when ice was present in all years/for all gcms
  
  # Determine surface, middle, and bottom depths for plotting
  plot_depths <- get_plotting_depths(glm_preds_long)

  # subset to plotting depths
  glm_preds_plot <- glm_preds_long %>%
    filter(depth %in% plot_depths)
  glm_mean_plot <- glm_mean_long %>%
    filter(depth %in% plot_depths)
  
  # Build plot
  ice_label <- ifelse(all_gcms == TRUE, 'any GCM ice', 'any year ice')
  ice_mean_label <- ifelse(all_gcms == TRUE, 'all GCMs ice', 'all years ice')
  plot_title <- ifelse(all_gcms == TRUE, 
                  paste(sprintf("%s", unique(glm_preds_long$site_id)), 'GCM: all', sep='\n'),
                  paste(sprintf("%s", unique(glm_preds_long$site_id)), sprintf('GCM: %s', unique(glm_preds_long$driver)), sep='\n'))
  doy_plot <- ggplot()+
    geom_line(data=glm_preds_plot, aes(x=doy, y=temperature, color='a_gcm_glm'), size=0.5, alpha=0.2, linetype='solid') +
    geom_line(data=glm_mean_plot, aes(x=doy, y=mean_temp, color="b_gcm_glm_mean"), size=0.5, alpha=1, linetype='solid') +
    geom_point(data = glm_preds_ice, aes(x=doy, y=temperature, color='c_gcm_glm_ice'), size=0.1, alpha=0.2, pch=16) +
    geom_point(data = glm_preds_ice_mean, aes(x=doy, y=mean_temp, color='d_gcm_glm_ice_mean'), size=0.1, alpha=0.3, pch=16) +
    scale_color_manual(values = c("cornflowerblue","midnightblue", "plum2","maroon4"), labels = c('GLM preds','mean of GLM preds', ice_label, ice_mean_label), guide= guide_legend(override.aes = list(alpha = c(1,1,1,1), linetype = c('solid','solid','blank','blank'), shape=c(NA,NA,16,16), size=c(0.5,0.5,0.5,0.5)))) +
    facet_grid(depth ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= plot_title,  y="Temperature (\u00b0C)") +
    theme_bw() +
    theme(
      strip.text = element_text(size=14),
      axis.text = element_text(size=12),
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      legend.text = element_text(size=14),
      legend.title = element_blank(),
      legend.position = "top"
    )
  
  # Save plot
  outfile <- ifelse(all_gcms == TRUE, 
         sprintf(outfile_template, unique(glm_preds_long$site_id)),
         sprintf(outfile_template, unique(glm_preds_long$site_id), unique(glm_preds_long$driver)))
  
  ggsave(filename=outfile, plot=doy_plot, dpi=300, width=10, height=6)
  return(outfile)
}

#' @title Plot predicted temperature profiles in a given lake, for all
#' 6 GCMs, across all years, for 4 specified dates
#' @description For a single lake, all 6 GCMs and all 3 time periods, 
#' plot temperature profiles for 4 specified DOY in each year, as well
#' as the average GLM profile for each of the 4 DOY within each 20-year
#' time period. The output plot is faceted by DOY and by time period
#' @param site_id The id for the current lake
#' @param glm_preds_long Long-formatted temperature predictions for the
#' current lake, for all GCMs (6 values for each depth on each calendar
#' date)
#' @param glm_mean_long Long-formatted mean temperature predictions for
#' each DOY across all years and all GCMs (single value for each depth 
#' on each doy in each time period)
#' @param plot_month_days The month-day combinations for which to plot
#' predicted temperature profiles
#' @param outfile_template The template filepath for the exported png, which
#' is customized with the site_id
#' @return The filepath of the exported png 
plot_20yr_average_profiles <- function(site_id, glm_preds_long, glm_mean_long, plot_month_days, outfile_template) {
  # modify the glm uncalibrated predictions dates
  plot_dates <- paste0('2021-', plot_month_days) #Assign random year to get DOY
  
  # subset predictions to selected DOYs
  glm_preds_plot <- glm_preds_long %>%
    filter(doy %in% yday(plot_dates))
  glm_mean_plot <- glm_mean_long %>%
    filter(doy %in% yday(plot_dates))

  # build plot  
  doy_plot <- ggplot()+
    geom_point(data= glm_preds_plot, aes(x=temperature, y=depth, color='a_gcm_glm'), size=0.5, alpha=0.17) + 
    geom_point(data=glm_mean_plot, aes(x=mean_temp, y=depth, color="b_gcm_glm_mean"), size=1, alpha=1) +
    scale_y_reverse(lim=c(max(glm_mean_plot$depth),0)) +
    scale_color_manual(values = c("cornflowerblue","midnightblue"), labels = c('GLM preds','mean of GLM preds'), guide= guide_legend(override.aes = list(alpha = c(0.17,1), size=c(0.5,1)))) +
    facet_grid(doy ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= paste(sprintf("%s -- GLM and mean GLM profiles", site_id), 
                      "\n GCM: all", 
                      sprintf("\n Dates: %s", paste(paste0('DOY ', yday(plot_dates), ':'), plot_month_days, collapse=', '))), 
         x="Temperature (\u00b0C)", 
         y="Depth (m)") +
    theme_bw() +
    theme(
      strip.text = element_text(size=14),
      axis.text = element_text(size=12),
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      legend.text = element_text(size=14),
      legend.title = element_blank(),
      legend.position = "top"
    )

  # save plot
  outfile <- sprintf(outfile_template, site_id)
  ggsave(filename=outfile, plot=doy_plot, dpi=300, width=10, height=6)
  return(outfile)
}
