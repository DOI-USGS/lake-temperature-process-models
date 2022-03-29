#' @title Get site-specifc GLM predictions
#' @description function to pull and combine GLM predictions for a single site
#' for all 3 simulation periods based on the input meteo file and export file 
#' vectors provided - may be pulling predictions for a single lake-gcm combo, 
#' or predictions for all GCMS for a single lake.
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
get_site_preds <- function(site_meteo_files, site_export_files, run_gcm_list) {
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
    mutate(date = as.Date(time), .before=1) %>%
    select(-time) %>%
    mutate("month" = month(date),
           "year"= year(date), 
           "doy"= yday(date),
           "decade" = sprintf('%ss', year - (year %% 10)),
           "period" = sprintf('%s - %s', year - (year %% 20), (year - (year %% 20)) + 20),
           .after=date) %>%
    filter(!(year == 2000)) %>% #Predictions for this year are incomplete and fall in a different 20-year period, so exclude
    select(date, month, year, doy, decade, period, gcm, ice, hice, evap, n_layers, everything()) # Place the temperature prediction columns last
  
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
#' 'temp_0', 'temp_0.5', etc.) to wide format, with a column for 'depth'
#' and a column for 'temperature'
#' @param input_wide The wide-formatted input data
#' @return a munged dataframe of GLM predictions that includes all of 
#' the columns in the original wide data except the 'temp_X' columns,
#' which are instead reduced to two columns: 'depth' and 'temperature'
munge_long <- function(input_wide) {
  input_long <- input_wide %>%
    pivot_longer(temp_0:colnames(input_wide)[ncol(input_wide)], names_to="depth", values_to="temperature") %>%
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
  n_lambda <- 1
  depth_scores <- predictions_long %>% group_by(depth) %>% tally() %>%
    mutate(
      n_score = n_lambda * n/max(n),
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
  
  # Get mean of GLM preds across all 6 GCMs
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
plot_20yr_average_preds_ice <- function(lake_group, outfile_template) {
  # Get glm preds for the current lake (wide format)
  glm_preds <- get_site_preds(lake_group$raw_meteo_fl, lake_group$export_fl, lake_group$gcm)

  # Munge them to long format
  glm_preds_long <- munge_long(glm_preds)
  
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
  glm_plot <- glm_preds_long %>%
    filter(depth %in% plot_depths)
  
  # Get mean of GLM predictions across all 6 GCMs
  glm_mean <- glm_plot %>%
    group_by(depth, doy, period) %>%
    summarize(mean_temp = mean(temperature, na.rm=TRUE))
  
  # Build plot
  doy_plot <- ggplot()+
    geom_line(data=glm_plot, aes(x=doy, y=temperature, color='a_gcm_glm'), size=0.5, alpha=0.2, linetype='solid') +
    geom_line(data=glm_mean, aes(x=doy, y=mean_temp, color="b_gcm_glm_mean"), size=0.5, alpha=1, linetype='solid') +
    geom_point(data = glm_preds_ice, aes(x=doy, y=temperature, color='c_gcm_glm_ice'), size=0.1, alpha=0.2, pch=16) +
    geom_point(data = glm_preds_ice_mean, aes(x=doy, y=mean_temp, color='d_gcm_glm_ice_mean'), size=0.1, alpha=0.3, pch=16) +
    scale_color_manual(values = c("cornflowerblue","midnightblue", "plum2","maroon4"), labels = c('GLM preds','mean of GLM preds', 'any GCM ice', 'all GCMs ice'), guide= guide_legend(override.aes = list(alpha = c(1,1,1,1), linetype = c('solid','solid','blank','blank'), shape=c(NA,NA,16,16), size=c(0.5,0.5,0.5,0.5)))) +
    facet_grid(depth ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= paste(sprintf("%s", unique(lake_group$site_id)), 'GCM: all', sep='\n'),  y="Temperature (\u00b0C)") +
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
  outfile <- sprintf(outfile_template, unique(lake_group$site_id))
  ggsave(filename=outfile, plot=doy_plot, dpi=300, width=10, height=6)
  return(outfile)
}

#' @title Plot predicted surface, middle, and bottom temperatures in a 
#' given lake, for all 6 GCMs
#' @description For a single lake, all 6 GCMs and all 3 time periods, 
#' plot temperature profiles for 4 specified DOY in each year, 
#' as well as the average GLM profile for each of the 4 DOY within each 
#' 20-year time period. The output plot is faceted by DOY and by period
#' @param lake_group a single group from `p4_subset_lake_groups`, which
#' is a filtered version of the `p2_glm_uncalibrated_lake_groups` tibble, 
#' which includes columns for site_id, gcm, time_period, raw_meteo_fl, 
#' export_fl, and export_fl_hash for *fully successful* runs. To limit the 
#' number of lake-specific plots that are generated, the `p4_subset_lake_groups`
#' was created by filtering `p2_glm_uncalibrated_lake_groups` to site ids 
#' within the manually specified `p4_plot_site_ids` vector, retaining the 
#' grouping by site_id. The function maps over these groups.
#' @param plot_month_days The month-day combinations for which to plot
#' predicted temperature profiles
#' @param outfile_template The template filepath for the exported png, which
#' is customized with the site_id
#' @return The filepath of the exported png 
plot_20yr_average_profiles <- function(lake_group, plot_month_days, outfile_template) {
  # get glm preds for the current lake (wide format)
  glm_preds <- get_site_preds(lake_group$raw_meteo_fl, lake_group$export_fl, lake_group$gcm)
  
  # munge them to long format
  glm_preds_long <- munge_long(glm_preds)
  
  # modify the glm uncalibrated predictions dates
  plot_dates <- paste0('2021-', plot_month_days) #Assign random year to get DOY
  
  # subset predictions to selected DOYs
  glm_plot <- glm_preds_long %>%
    filter(doy %in% yday(plot_dates)) %>%
    filter(!(year == 2000))
  
  # get mean of GLM predictions for each DOY, across all 6 GCMs
  glm_mean <- glm_plot %>%
    group_by(depth, doy, period) %>%
    summarize(mean_temp = mean(temperature, na.rm=TRUE)) %>%
    filter(!is.na(mean_temp))

  # build plot  
  doy_plot <- ggplot()+
    geom_point(data= glm_plot, aes(x=temperature, y=depth, color='a_gcm_glm'), size=0.5, alpha=0.17) + #line=gcm, 
    geom_point(data=glm_mean, aes(x=mean_temp, y=depth, color="b_gcm_glm_mean"), size=1, alpha=1) +
    scale_y_reverse(lim=c(max(glm_mean$depth),0)) +
    scale_color_manual(values = c("cornflowerblue","midnightblue"), labels = c('GLM preds','mean of GLM preds'), guide= guide_legend(override.aes = list(alpha = c(0.17,1), size=c(0.5,1)))) +
    facet_grid(doy ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= paste(sprintf("%s -- GLM and mean GLM profiles", unique(lake_group$site_id)), 
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
  outfile <- sprintf(outfile_template, unique(lake_group$site_id))
  ggsave(filename=outfile, plot=doy_plot, dpi=300, width=10, height=6)
  return(outfile)
}
