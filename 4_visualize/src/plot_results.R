#' @title Create a violin chart of predicted surface, middle, and bottom 
#' temperatures in all lakes, across GCMs
#' @description Plot the distribution of predicted temperatures
#' across all lakes, broken out by depth class (surface, middle or bottom),
#' gcm, and time period. The output plot is faceted by GCM and by period
#' @param run_groups The `p2_glm_uncalibrated_run_groups` grouped version 
#' of the `p2_glm_uncalibrated_runs` output tibble subset to the site_id, 
#' gcm, time_period, raw_meteo_fl, export_fl, and export_fl_hash columns 
#' and grouped by site_id and gcm. Then filtered to only groups for which 
#' glm_success==TRUE for all runs in that group.
#' @param outfile The filepath for the exported png
#' @return The filepath of the exported png 
plot_temp_violin_gcms <- function(run_groups, outfile) {
  # Get surface, middle, and bottom predictions for each lake, across all GCMs
  glm_preds <- get_surface_middle_bottom_preds(run_groups)
  
  # Build plot
  violin_plot <- ggplot(glm_preds) + 
    geom_violin(aes(x=depth_class, y=temperature,color=depth_class), trim=FALSE) + 
    facet_grid(period~gcm) +
    labs(title = paste('Distribution of temperatures across depth classes, by gcm and time period', 
                       sprintf('number of lakes: %s', length(unique(glm_preds$site_id))), sep='\n'))
  
  # Save plot
  ggsave(filename=outfile, plot=violin_plot, dpi=300, width=15, height=6)
  return(outfile)
}

#' @title Create a violin chart of predicted surface, middle, and bottom 
#' temperatures in all lakes for all GCMs, across (even) months
#' @description Plot the distribution of predicted temperatures
#' across all lakes, broken out by depth class (surface, middle or bottom),
#' by month (only even months are plotted currently), and time period.
#' The output plot is faceted by month and by period
#' @param run_groups The `p2_glm_uncalibrated_run_groups` grouped version 
#' of the `p2_glm_uncalibrated_runs` output tibble subset to the site_id, 
#' gcm, time_period, raw_meteo_fl, export_fl, and export_fl_hash columns 
#' and grouped by site_id and gcm. Then filtered to only groups for which 
#' glm_success==TRUE for all runs in that group.
#' @param outfile The filepath for the exported png
#' @return The filepath of the exported png 
plot_temp_violin_months <- function(run_groups, outfile) {
  # Get surface, middle, and bottom predictions for each lake, across all GCMs
  glm_preds <- get_surface_middle_bottom_preds(run_groups)
  
  # filter to only even months
  # and get mean temperature predictions across GCMs
  glm_preds <- glm_preds %>%
    filter(month %% 2 == 0) %>% 
    group_by(site_id, date, month, period, depth_class, depth) %>% 
    summarize(mean_temperature=mean(temperature, na.rm=TRUE))
  
  # Build plot
  violin_plot <- ggplot(glm_preds) + 
    geom_violin(aes(x=depth_class, y=mean_temperature,color=depth_class), trim=FALSE) + 
    facet_grid(period~month) +
    labs(title = paste('Distribution of temperatures across depth classes, by (even) months and time period', 
                       sprintf('number of lakes: %s', length(unique(glm_preds$site_id))), sep='\n'))
  
  # Save plot
  ggsave(filename=outfile, plot=violin_plot, dpi=300, width=15, height=6)
  return(outfile)
}

#' @title Plot predicted surface, middle, and bottom temperatures in a 
#' given lake, for a given GCM
#' @description For a single lake, a single GCM and all 3 time periods, 
#' plot surface, middle, and bottom temperature predictions in each year, 
#' by DOY, as well as the average GLM prediction for each DOY in each 
#' 20-year time period. Also plot whether or not the model predicted ice 
#' on a given DOY. The output plot is faceted by period and by depth class
#' @param run_group a single group from `p4_subset_run_groups`, which
#' is a filtered version of the `p2_glm_uncalibrated_run_groups` tibble, 
#' which includes columns for site_id, gcm, time_period, raw_meteo_fl, 
#' export_fl, and export_fl_hash for *fully successful* runs. To limit the 
#' number of lake-specific plots that are generated, the `p4_subset_run_groups`
#' was created by filtering `p2_glm_uncalibrated_run_groups` to site ids 
#' within the manually specified `p4_plot_site_ids` vector, retaining the 
#' grouping by site_id and gcm. The function maps over these groups.
#' @param outfile_template The template filepath for the exported png, which
#' is customized with the site_id and the name of the GCM for which predictions
#' are plotted
#' @return The filepath of the exported png 
plot_20yr_gcm_preds_ice <- function(run_group, outfile_template) {
  # Get glm preds for the current lake (wide format)
  glm_preds <- get_site_preds(run_group$raw_meteo_fl, run_group$export_fl, run_group$gcm)
  
  # Pull out ice predictions, and create offset temperature for plotting 
  glm_preds_ice <- glm_preds %>%
    select(date, doy, ice, temp_0) %>%
    rename(temperature = temp_0) %>% # could use actual surface temp
    mutate(temperature = -1) %>% # artificially set to -1 so doesn't block surface temp pred
    mutate(depth = 0) %>%
    filter(ice ==TRUE) %>%
    arrange(date, depth)

  # Munge long
  glm_preds_long <- munge_long(glm_preds)
  
  # Determine surface, middle, and bottom depths for plotting
  plot_depths <- get_plotting_depths(glm_preds_long)

  # Filter predictions to plotting depths
  glm_plot <- glm_preds_long %>%
    filter(depth %in% plot_depths)
  
  # Get mean of GLM preds across all years
  glm_mean <- glm_plot %>%
    group_by(depth, doy, period) %>%
    summarize(mean_temp = mean(temperature, na.rm=TRUE))
  
  # Build plot  
  doy_plot <- ggplot()+
    geom_line(data=glm_plot, aes(x=doy, y=temperature, color='a_gcm_glm'), size=0.5, alpha=0.2, linetype='solid') +
    geom_line(data=glm_mean, aes(x=doy, y=mean_temp, color="b_gcm_glm_mean"), size=0.5, alpha=1, linetype='solid') +
    geom_point(data = glm_preds_ice, aes(x=doy, y=temperature, color='c_gcm_glm_ice'), size=0.1, alpha=0.05, pch=16) +
    scale_color_manual(values = c("cornflowerblue","midnightblue", "maroon4"), labels = c('GLM preds','mean of GLM preds', 'GLM ice'), guide= guide_legend(override.aes = list(alpha = c(1,1,1), linetype = c('solid','solid','blank'), shape=c(NA,NA,16), size=c(0.5,0.5,0.5)))) +
    facet_grid(depth ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= paste(sprintf("%s", unique(run_group$site_id)), sprintf('GCM: %s', unique(run_group$gcm)), sep='\n'),  y="Temperature (\u00b0C)") +
    theme_bw() +
    theme(
      panel.border = element_blank(),
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
  outfile <- sprintf(outfile_template, unique(run_group$site_id), unique(run_group$gcm))
  ggsave(filename=outfile, plot=doy_plot, dpi=300, width=10, height=6)
  return(outfile)
}

#' @title Plot predicted surface, middle, and bottom temperatures in a 
#' given lake, for all 6 GCMs
#' @description For a single lake, all 6 GCMs and all 3 time periods, 
#' plot surface, middle, and bottom temperature predictions in each year, 
#' by DOY, as well as the average GLM prediction for each DOY in each 
#' 20-year time period. Also plot whether or not the model predicted ice 
#' on a given DOY for any GCM or for all 6 GCMs. The output plot is 
#' faceted by period and by depth class
#' @param lake_group a single group from `p4_subset_lake_groups`, which
#' is a filtered version of the `p2_glm_uncalibrated_lake_groups` tibble, 
#' which includes columns for site_id, gcm, time_period, raw_meteo_fl, 
#' export_fl, and export_fl_hash for *fully successful* runs. To limit the 
#' number of lake-specific plots that are generated, the `p4_subset_lake_groups`
#' was created by filtering `p2_glm_uncalibrated_lake_groups` to site ids 
#' within the manually specified `p4_plot_site_ids` vector, retaining the 
#' grouping by site_id. The function maps over these groups.
#' @param outfile_template The template filepath for the exported png, which
#' is customized with the site_id
#' @return The filepath of the exported png 
plot_20yr_average_preds_ice <- function(site_id, glm_preds, glm_preds_long, glm_mean_long, outfile_template) {

  # Pull out ice predictions, and create offset temperature for plotting
  glm_preds_ice <- glm_preds %>%
    select(gcm, year, doy, period, date, ice, temp_0) %>%
    rename(temperature = temp_0) %>% # could use actual surface temp
    mutate(temperature = -1) %>% # artificially set to -1 so doesn't block surface temp pred
    mutate(depth = 0) %>%
    filter(!(year == 2000))
  
  # Get dates when all GCMs predicted ice
  glm_preds_ice_mean <- glm_preds_ice %>%
    group_by(depth, doy, period) %>%
    summarize(ice_all_gcms = ifelse(sum(ice)==n(), TRUE, FALSE), mean_temp = -2) %>%
    filter(ice_all_gcms ==TRUE)

  # Filter ice predictions to only those dates when ice was present
  glm_preds_ice <- glm_preds_ice %>%
    filter(ice==TRUE)
  
  # Determine surface, middle, and bottom depths for plotting
  plot_depths <- get_plotting_depths(glm_preds_long)

  # subset to plotting depths
  glm_preds_plot <- glm_preds_long %>%
    filter(depth %in% plot_depths)
  glm_mean_plot <- glm_mean_long %>%
    filter(depth %in% plot_depths)
  
  # # Get mean of GLM predictions across all years and all 6 GCMs
  # glm_mean <- glm_plot %>%
  #   group_by(depth, doy, period) %>%
  #   summarize(mean_temp = mean(temperature, na.rm=TRUE))
  
  # Build plot
  doy_plot <- ggplot()+
    geom_line(data=glm_preds_plot, aes(x=doy, y=temperature, color='a_gcm_glm'), size=0.5, alpha=0.2, linetype='solid') +
    geom_line(data=glm_mean_plot, aes(x=doy, y=mean_temp, color="b_gcm_glm_mean"), size=0.5, alpha=1, linetype='solid') +
    geom_point(data = glm_preds_ice, aes(x=doy, y=temperature, color='c_gcm_glm_ice'), size=0.1, alpha=0.2, pch=16) +
    geom_point(data = glm_preds_ice_mean, aes(x=doy, y=mean_temp, color='d_gcm_glm_ice_mean'), size=0.1, alpha=0.3, pch=16) +
    scale_color_manual(values = c("cornflowerblue","midnightblue", "plum2","maroon4"), labels = c('GLM preds','mean of GLM preds', 'any GCM ice', 'all GCMs ice'), guide= guide_legend(override.aes = list(alpha = c(1,1,1,1), linetype = c('solid','solid','blank','blank'), shape=c(NA,NA,16,16), size=c(0.5,0.5,0.5,0.5)))) +
    facet_grid(depth ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= paste(sprintf("%s", site_id), 'GCM: all', sep='\n'),  y="Temperature (\u00b0C)") +
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
  outfile <- sprintf(outfile_template, site_id)
  ggsave(filename=outfile, plot=doy_plot, dpi=300, width=10, height=6)
  return(outfile)
}

#' @title Plot predicted surface, middle, and bottom temperatures in a 
#' given lake, for all 6 GCMs
#' @description For a single lake, all 6 GCMs and all 3 time periods, 
#' plot temperature profiles for 4 specified DOY in each year, 
#' as well as the average GLM profile for each of the 4 DOY within each 
#' 20-year time period. The output plot is faceted by DOY and by period
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
    geom_point(data= glm_preds_plot, aes(x=temperature, y=depth, color='a_gcm_glm'), size=0.5, alpha=0.17) + #line=gcm, 
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
