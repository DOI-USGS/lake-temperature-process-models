get_site_preds <- function(site_meteo_files, site_export_files, run_gcm_list) {
  site_preds <- purrr::pmap_df(list(site_meteo_files, site_export_files, run_gcm_list), function(raw_meteo_fl, export_file, gcm) {
    # Define time period begin and end dates from raw meteo_fl
    meteo_data <- arrow::read_feather(raw_meteo_fl, col_select = "time")
    begin <- min(meteo_data$time)
    end <- max(meteo_data$time)
    # read in data for that time period and truncate
    arrow::read_feather(export_file) %>%
      filter(time >= as.Date(begin) & time <= as.Date(end)) %>%
      mutate(driver_type = gcm)
  }) %>%
    mutate(date = as.Date(time), .before=1) %>%
    select(-time) %>%
    mutate("month" = month(date),
           "year"= year(date), 
           "doy"= yday(date),
           "decade" = sprintf('%ss', year - (year %% 10)),
           "period" = sprintf('%s - %s', year - (year %% 20), (year - (year %% 20)) + 20),
           .after=date) %>%
    filter(!(year == 2000)) %>%
    select(date, month, year, doy, decade, period, driver_type, ice, everything())
  
  return(site_preds)
}

get_surface_middle_bottom_preds <- function(run_groups) {
  surface_middle_bottom_preds <- run_groups %>%
    group_by(site_id, gcm) %>%
    group_modify(~ {
      site_meteo_files <- .x$raw_meteo_fl
      site_export_files <- .x$export_fl
      site_preds <- get_site_preds(site_meteo_files, site_export_files, rep(.y$gcm, length(site_export_files)))
      
      site_preds_long <- munge_long(site_preds)
      
      plot_depths<- get_plotting_depths(site_preds_long)
      
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

munge_long <- function(input_wide) {
  input_long <- input_wide %>%
    pivot_longer(temp_0:colnames(input_wide)[ncol(input_wide)], names_to="depth", values_to="temperature") %>%
    mutate(depth = as.numeric(str_remove(depth, 'temp_'))) %>%
    arrange(date, depth)
}

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
  plot_depth_bottom = unname(depths['bottom'])-0.5
  return(list(plot_depth_surface=plot_depth_surface, plot_depth_middle=plot_depth_middle, plot_depth_bottom=plot_depth_bottom))
}

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

plot_temp_violin_months <- function(run_groups, outfile) {
  # Get surface, middle, and bottom predictions for each lake, across all GCMs
  glm_preds <- get_surface_middle_bottom_preds(run_groups)
  
  # filter to only even months
  # and get mean temperature prediction across GCMs
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
    labs(title= paste(sprintf("%s", run_group$lake_id), sprintf('GCM: %s', run_group$gcm), sep='\n'),  y="Temperature (\u00b0C)") +
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

plot_20yr_average_preds_ice <- function(lake_group, outfile_template) {
  # Get glm preds for the current lake (wide format)
  glm_preds <- get_site_preds(lake_group$raw_meteo_fl, lake_group$export_fl, lake_group$gcm)

  # Munge them to long format
  glm_preds_long <- munge_long(glm_preds)
  
  # Pull out ice predictions, and create offset temperature for plotting
  glm_preds_ice <- glm_preds %>%
    select(driver_type, year, doy, period, date, ice, temp_0) %>%
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
    geom_point(data= glm_plot, aes(x=temperature, y=depth, color='a_gcm_glm'), size=0.5, alpha=0.17) + #line=driver_type, 
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
