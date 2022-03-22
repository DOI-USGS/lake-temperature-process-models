# glm_preds <- run_groups %>%
#   group_by(site_id, gcm) %>%
#   group_modify(~ {
#     site_meteo_files <- .x$raw_meteo_fl
#     site_export_files <- .x$export_fl
#     purrr::map2_df(site_meteo_files, site_export_files, function(raw_meteo_fl, export_file) {
#       # Define time period begin and end dates from raw meteo_fl
#       meteo_data <- arrow::read_feather(raw_meteo_fl, col_select = "time")
#       begin <- min(meteo_data$time)
#       end <- max(meteo_data$time)
#       # read in data for that time period and truncate
#       arrow::read_feather(export_file) %>%
#         filter(time >= as.Date(begin) & time <= as.Date(end))
#     }) %>%
#       select(ice, everything())
#   }) %>%
#   mutate(date = as.Date(time), .before=1) %>%
#   select(-time) %>%
#   mutate("year"= year(date), "doy"= yday(date),
#          "decade" = sprintf('%ss', year - (year %% 10)),
#          "period" = sprintf('%s - %s', year - (year %% 20), (year - (year %% 20)) + 20),
#          .after=date) %>%
#   filter(!(year == 2000))
# 
# glm_preds_long <- munge_long(glm_preds)

plot_temp_density_gcms <- function(run_groups, outfile) {
  glm_preds <- run_groups %>%
    group_by(site_id, gcm) %>%
    group_modify(~ {
      site_meteo_files <- .x$raw_meteo_fl
      site_export_files <- .x$export_fl
      site_preds <- get_site_preds(site_meteo_files, site_export_files)
      
      site_preds_long <- munge_long(site_preds)
      
      plot_depths<- get_plotting_depths(site_preds_long)
      
      # ADD COUNT OF LAKES TO PLOT?
      filtered_preds <- filter(site_preds_long, depth %in% plot_depths) %>%
        mutate(depth_class=case_when(
          depth==plot_depths$plot_depth_surface ~ 'surface',
          depth==plot_depths$plot_depth_middle ~ 'middle',
          depth==plot_depths$plot_depth_bottom ~ 'bottom'
        ), .before=depth)
      return(filtered_preds)
    })
  
  density_plot <- ggplot(glm_preds) + 
    geom_density(aes(x=temperature,color=depth_class)) + 
    facet_grid(gcm~period) +
    labs(title = paste('Distribution of temperatures across depth classes, by time period and GCM', 
                       sprintf('number of lakes: %s', length(unique(glm_preds$site_id))), sep='\n'))
  
  ggsave(filename=outfile, plot=density_plot, dpi=300, width=10, height=6)
  return(outfile)
}

plot_temp_violin_gcms <- function(run_groups, outfile) {
  glm_preds <- run_groups %>%
    group_by(site_id, gcm) %>%
    group_modify(~ {
      site_meteo_files <- .x$raw_meteo_fl
      site_export_files <- .x$export_fl
      site_preds <- get_site_preds(site_meteo_files, site_export_files)
      
      site_preds_long <- munge_long(site_preds)
      
      plot_depths<- get_plotting_depths(site_preds_long)
      
      # ADD COUNT OF LAKES TO PLOT?
      filtered_preds <- filter(site_preds_long, depth %in% plot_depths) %>%
        mutate(depth_class=case_when(
          depth==plot_depths$plot_depth_surface ~ 'surface',
          depth==plot_depths$plot_depth_middle ~ 'middle',
          depth==plot_depths$plot_depth_bottom ~ 'bottom'
        ), .before=depth)
      return(filtered_preds)
    })
  
  violin_plot <- ggplot(glm_preds) + 
    geom_violin(aes(x=depth_class, y=temperature,color=depth_class)) + 
    facet_grid(period~gcm) +
    labs(title = paste('Distribution of temperatures across depth classes, by gcm and time period', 
                       sprintf('number of lakes: %s', length(unique(glm_preds$site_id))), sep='\n'))
  
  ggsave(filename=outfile, plot=violin_plot, dpi=300, width=15, height=6)
  return(outfile)
}

get_site_preds <- function(site_meteo_files, site_export_files) {
  site_preds <- purrr::map2_df(site_meteo_files, site_export_files, function(raw_meteo_fl, export_file) {
    # Define time period begin and end dates from raw meteo_fl
    meteo_data <- arrow::read_feather(raw_meteo_fl, col_select = "time")
    begin <- min(meteo_data$time)
    end <- max(meteo_data$time)
    # read in data for that time period and truncate
    arrow::read_feather(export_file) %>%
      filter(time >= as.Date(begin) & time <= as.Date(end))
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
    select(date, month, year, decade, period, ice, everything())
  
  return(site_preds)
}

plot_temp_density_months <- function(run_groups, outfile) {
  glm_preds <- run_groups %>%
    group_by(site_id, gcm) %>%
    group_modify(~ {
      site_meteo_files <- .x$raw_meteo_fl
      site_export_files <- .x$export_fl
      site_preds <- get_site_preds(site_meteo_files, site_export_files)
      
      site_preds_long <- munge_long(site_preds)

      plot_depths<- get_plotting_depths(site_preds_long)
      
      # ADD COUNT OF LAKES TO PLOT
      # FURTHER SPLIT PLOT BY TIME OF YEAR, OR MONTH?
      filtered_preds <- filter(site_preds_long, depth %in% plot_depths) %>%
        mutate(depth_class=case_when(
          depth==plot_depths$plot_depth_surface ~ 'surface',
          depth==plot_depths$plot_depth_middle ~ 'middle',
          depth==plot_depths$plot_depth_bottom ~ 'bottom'
        ), .before=depth) %>%
      return(filtered_preds)
    })
  
  # filter to only even months
  # and get mean temperature prediction across GCMs
  glm_preds <- glm_preds %>%
    filter(month %% 2 == 0) %>% 
    group_by(site_id, date, month, period, depth_class, depth) %>% 
    summarize(mean_temperature=mean(temperature))

  density_plot <- ggplot(glm_preds) + 
    geom_histogram(aes(x=mean_temperature,color=depth_class)) + 
    facet_grid(month~period) +
    labs(title = paste('Distribution of temperatures across depth classes, by time period and (even) months', 
                       sprintf('number of lakes: %s', length(unique(glm_preds$site_id))), sep='\n'))
  browser()
  ggsave(filename=outfile, plot=density_plot, dpi=300, width=10, height=6)
  return(outfile)
}

plot_temp_violin_months <- function(run_groups, outfile) {
  glm_preds <- run_groups %>%
    group_by(site_id, gcm) %>%
    group_modify(~ {
      site_meteo_files <- .x$raw_meteo_fl
      site_export_files <- .x$export_fl
      site_preds <- get_site_preds(site_meteo_files, site_export_files)
      
      site_preds_long <- munge_long(site_preds)
      
      plot_depths<- get_plotting_depths(site_preds_long)
      
      # ADD COUNT OF LAKES TO PLOT
      # FURTHER SPLIT PLOT BY TIME OF YEAR, OR MONTH?
      filtered_preds <- filter(site_preds_long, depth %in% plot_depths) %>%
        mutate(depth_class=case_when(
          depth==plot_depths$plot_depth_surface ~ 'surface',
          depth==plot_depths$plot_depth_middle ~ 'middle',
          depth==plot_depths$plot_depth_bottom ~ 'bottom'
        ), .before=depth) %>%
        return(filtered_preds)
    })
  
  # filter to only even months
  # and get mean temperature prediction across GCMs
  glm_preds <- glm_preds %>%
    filter(month %% 2 == 0) %>% 
    group_by(site_id, date, month, period, depth_class, depth) %>% 
    summarize(mean_temperature=mean(temperature))
  
  violin_plot <- ggplot(glm_preds) + 
    geom_violin(aes(x=depth_class, y=mean_temperature,color=depth_class)) + 
    facet_grid(period~month) +
    labs(title = paste('Distribution of temperatures across depth classes, by (even) months and time period', 
                       sprintf('number of lakes: %s', length(unique(glm_preds$site_id))), sep='\n'))
  
  ggsave(filename=outfile, plot=violin_plot, dpi=300, width=15, height=6)
  return(outfile)

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

# output_files <- list.files('2_run/out/Denali','*.feather')
# tar_load(p1_gcm_names)
# lake_ids <- c('nhdhr_91597227','nhdhr_72863673','nhdhr_59724783','nhdhr_47719227','nhdhr_45361793',
#               'nhdhr_166867643','nhdhr_120020627','nhdhr_120018623','nhdhr_114542327','nhdhr_114539927')
# gcm_names <- p1_gcm_names 

# model_runs <- tibble(
#   output_file = output_files,
#   gcm = stringr::str_extract(output_file, paste(gcm_names,collapse="|")),
#   lake_id = stringr::str_extract(output_file, paste(lake_ids,collapse="|")))

munge_long <- function(input_wide) {
  input_long <- input_wide %>%
    pivot_longer(temp_0:colnames(input_wide)[ncol(input_wide)], names_to="depth", values_to="temperature") %>%
    mutate(depth = as.numeric(str_remove(depth, 'temp_'))) %>%
    arrange(date, depth)
}

get_glm_preds <- function(site, gcm, format='long') {
  glm_preds <- arrow::read_feather(sprintf('2_run/out/Denali/GLM_%s_%s.feather', site, gcm)) %>%
    mutate(date = as.Date(time), ice_flag=ice, .before=1) %>%
    select(-time, -ice)
  if (format == 'long') {
    glm_preds <- munge_long(glm_preds)
  }
  glm_preds <- glm_preds %>%
    mutate(driver_type = gcm,"year"= year(date), "doy"= yday(date),
           "period" = sprintf('%s - %s', year - (year %% 20), (year - (year %% 20)) + 20), .before=1)
  return(glm_preds)
}


plot_20yr_preds <- function(run_group, outfile_template) {
  glm_preds <- purrr::map2_df(run_group$raw_meteo_fl, run_group$export_fl, function(raw_meteo_fl, export_file) {
    # Define time period begin and end dates from raw meteo_fl
    meteo_data <- arrow::read_feather(raw_meteo_fl, col_select = "time")
    begin <- min(meteo_data$time)
    end <- max(meteo_data$time)
    # read in data for that time period and truncate
    arrow::read_feather(export_file) %>%
      filter(time >= as.Date(begin) & time <= as.Date(end))
  }) %>%
    mutate(date = as.Date(time), .before=1) %>%
    select(-time)
  
  glm_preds_ice <- glm_preds %>%
    select(date, ice, temp_0) %>%
    rename(temperature = temp_0) %>% # could use actual surface temp
    mutate(temperature = -1) %>% # artificially set to -1 so doesn't block surface temp pred
    mutate(depth = 0) %>%
    mutate("year"= year(date), "doy"= yday(date), .before=1) %>%
    mutate("decade" = sprintf('%ss', year - (year %% 10))) %>%
    mutate("period" = sprintf('%s - %s', year - (year %% 20), (year - (year %% 20)) + 20)) %>%
    filter(!(year == 2000)) %>%
    filter(ice ==TRUE) %>%
    arrange(date, depth)
  
  # return(glm_preds_ice)
  
  glm_preds <- glm_preds %>%
    select(-ice)
  
  glm_preds_long <- munge_long(glm_preds)
  
  n_lambda <- 1
  depth_scores <- glm_preds_long %>% group_by(depth) %>% tally() %>%
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
  depth_column_surface = paste0('temp_',plot_depth_surface)
  depth_column_middle = paste0('temp_',plot_depth_middle)
  depth_column_bottom = paste0('temp_',plot_depth_bottom)
  
  # subset the glm uncalibrated predictions dataset
  glm_subset <- glm_preds %>%
    mutate("year"= year(date), "doy"= yday(date), .before=1) %>%
    mutate("decade" = sprintf('%ss', year - (year %% 10))) %>%
    mutate("period" = sprintf('%s - %s', year - (year %% 20), (year - (year %% 20)) + 20)) %>%
    filter(!(year == 2000)) %>%
    select(date, year, doy, decade, period, !!depth_column_surface, !!depth_column_middle, !!depth_column_bottom)
  
  glm_plot <- glm_subset %>%
    gather(depth, temperature, colnames(glm_subset)[6]:colnames(glm_subset)[ncol(glm_subset)]) %>%
    mutate(depth = as.numeric(str_remove(depth, 'temp_'))) %>%
    arrange(date, depth)
  
  glm_mean <- glm_plot %>%
    group_by(depth, doy, period) %>%
    summarize(mean_temp = mean(temperature))
  
  
  options(repr.plot.width=10, repr.plot.height=6)
  
  doy_plot <- ggplot()+
    geom_line(data=glm_plot, aes(x=doy, y=temperature, color='a_gcm_glm'), size=0.5, alpha=0.2, linetype='solid') +
    geom_line(data=glm_mean, aes(x=doy, y=mean_temp, color="b_gcm_glm_mean"), size=0.5, alpha=1, linetype='solid') +
    geom_point(data = glm_preds_ice, aes(x=doy, y=temperature, color='c_gcm_glm_ice'), size=0.1, alpha=0.05, pch=16) +
    scale_color_manual(values = c("cornflowerblue","midnightblue", "maroon4"), labels = c('GLM preds','mean of GLM preds', 'GLM ice'), guide= guide_legend(override.aes = list(alpha = c(1,1,1), linetype = c('solid','solid','blank'), shape=c(NA,NA,16), size=c(0.5,0.5,0.5)))) +
    facet_grid(depth ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= paste(sprintf("%s", run_group$lake_id), sprintf('GCM: %s', run_group$gcm), sep='\n'),  y="Temperature (°C)") +
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
  
  outfile <- sprintf(outfile_template, unique(run_group$site_id), unique(run_group$gcm))
  
  ggsave(filename=outfile, plot=doy_plot, dpi=300, width=10, height=6)
  return(outfile)
}
# plot_20yr_preds(test)
# purrr::pmap(model_runs, function(...) {
#   model_run <- tibble(...) 
#   plot_20yr_preds(model_run)
# })




plot_20yr_average_preds <- function(selected_site) {
  
  message(print(sprintf('current site: %s', selected_site)))
  
  glm_preds_access <- get_glm_preds(selected_site, 'ACCESS', format='wide')
  glm_preds_gfdl <- get_glm_preds(selected_site, 'GFDL', format='wide')
  glm_preds_cnrm <- get_glm_preds(selected_site, 'CNRM', format='wide')
  glm_preds_ipsl <- get_glm_preds(selected_site, 'IPSL', format='wide')
  glm_preds_mri <- get_glm_preds(selected_site, 'MRI', format='wide')
  glm_preds_miroc5 <- get_glm_preds(selected_site, 'MIROC5', format='wide')
  
  glm_preds <- bind_rows(glm_preds_access, glm_preds_gfdl, glm_preds_cnrm, glm_preds_ipsl, glm_preds_mri, glm_preds_miroc5)
  
  glm_preds_long <- munge_long(glm_preds)
  
  glm_preds_ice <- glm_preds %>%
    select(driver_type, year, doy, period, date, ice_flag, temp_0) %>%
    rename(temperature = temp_0) %>% # could use actual surface temp
    mutate(temperature = -1) %>% # artificially set to -1 so doesn't block surface temp pred
    mutate(depth = 0) %>%
    filter(!(year == 2000))
  
  glm_preds_ice_mean <- glm_preds_ice %>%
    group_by(depth, doy, period) %>%
    summarize(ice_all_gcms = ifelse(sum(ice_flag)==n(), TRUE, FALSE), mean_temp = -2) %>%
    filter(ice_all_gcms ==TRUE)
  # return(glm_preds_ice_mean)
  
  glm_preds_ice <- glm_preds_ice %>%
    filter(ice_flag==TRUE)
  
  n_lambda <- 1
  depth_scores <- glm_preds_long %>% group_by(depth) %>% tally() %>%
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
  depth_column_surface = paste0('temp_',plot_depth_surface)
  depth_column_middle = paste0('temp_',plot_depth_middle)
  depth_column_bottom = paste0('temp_',plot_depth_bottom)
  
  
  # subset the glm uncalibrated predictions dataset
  glm_subset <- glm_preds %>%
    select(driver_type, date, year, doy, period, depth_column_surface, depth_column_middle, depth_column_bottom) %>%
    filter(!(year == 2000))
  
  # modify the glm uncalibrated predictions dataset for plotting
  glm_plot <- glm_subset %>%
    gather(depth, temperature, colnames(glm_subset)[6]:colnames(glm_subset)[ncol(glm_subset)]) %>%
    mutate(depth = as.numeric(str_remove(depth, 'temp_'))) %>%
    arrange(date, depth)
  
  # return(glm_plot)
  
  glm_mean <- glm_plot %>%
    group_by(depth, doy, period) %>%
    summarize(mean_temp = mean(temperature))
  
  #     return(glm_subset_nldas)
  
  options(repr.plot.width=20, repr.plot.height=15)
  
  doy_plot <- ggplot()+
    geom_line(data=glm_plot, aes(x=doy, y=temperature, color='a_gcm_glm'), size=0.5, alpha=0.2, linetype='solid') +
    geom_line(data=glm_mean, aes(x=doy, y=mean_temp, color="b_gcm_glm_mean"), size=0.5, alpha=1, linetype='solid') +
    geom_point(data = glm_preds_ice, aes(x=doy, y=temperature, color='c_gcm_glm_ice'), size=0.1, alpha=0.2, pch=16) +
    geom_point(data = glm_preds_ice_mean, aes(x=doy, y=mean_temp, color='d_gcm_glm_ice_mean'), size=0.1, alpha=0.3, pch=16) +
    scale_color_manual(values = c("cornflowerblue","midnightblue", "plum2","maroon4"), labels = c('GLM preds','mean of GLM preds', 'any GCM ice', 'all GCMs ice'), guide= guide_legend(override.aes = list(alpha = c(1,1,1,1), linetype = c('solid','solid','blank','blank'), shape=c(NA,NA,16,16), size=c(0.5,0.5,0.5,0.5)))) +
    # scale_color_manual(values = c("cornflowerblue","midnightblue", "maroon4"), labels = c('GLM preds','mean of GLM preds', 'GLM pred ice'), guide= guide_legend(override.aes = list(alpha = c(1,1,1), linetype = c('solid','solid','blank'), shape=c(NA,NA,16), size=c(0.5,0.5,0.5)))) +
    facet_grid(depth ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= paste(sprintf("%s", selected_site), 'GCM: all', sep='\n'),  y="Temperature (°C)") +
    theme_bw() +
    theme(
      #             panel.border = element_blank(),
      strip.text = element_text(size=14),
      axis.text = element_text(size=12),
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      legend.text = element_text(size=14),
      legend.title = element_blank(),
      legend.position = "top"
    )
  
  # return(doy_plot)
  
  plot_filepath <- sprintf('4_visualize/out/20yr_preds_%s.png', selected_site)
  ggsave(filename=plot_filepath, plot=doy_plot, dpi=300, width=10, height=6)
}

# plot_20yr_average_preds(lake_ids[1])
# purrr::map(lake_ids, function(lake_id) {
#   plot_20yr_average_preds(lake_id)
# })

plot_20yr_average_profiles <- function(selected_site) {
  
  glm_preds_access <- get_glm_preds(selected_site, 'ACCESS', format='long')
  glm_preds_gfdl <- get_glm_preds(selected_site, 'GFDL', format='long')
  glm_preds_cnrm <- get_glm_preds(selected_site, 'CNRM', format='long')
  glm_preds_ipsl <- get_glm_preds(selected_site, 'IPSL', format='long')
  glm_preds_mri <- get_glm_preds(selected_site, 'MRI', format='long')
  glm_preds_miroc5 <- get_glm_preds(selected_site, 'MIROC5', format='long')
  
  glm_preds <- bind_rows(glm_preds_access, glm_preds_gfdl, glm_preds_cnrm, glm_preds_ipsl, glm_preds_mri, glm_preds_miroc5)
  
  plot_dates = c('2021-02-15','2021-05-15','2021-08-15','2021-11-15')
  
  
  # modify the glm uncalibrated predictions datas
  glm_plot <- glm_preds %>%
    filter(doy %in% yday(plot_dates)) %>%
    filter(!(year == 2000))
  glm_mean <- glm_plot %>%
    group_by(depth, doy, period) %>%
    summarize(mean_temp = mean(temperature)) %>%
    filter(!is.na(mean_temp))
  
  # return(glm_mean)
  
  options(repr.plot.width=20, repr.plot.height=15)
  
  doy_plot <- ggplot()+
    geom_point(data= glm_plot, aes(x=temperature, y=depth, color='a_gcm_glm'), size=0.5, alpha=0.17) + #line=driver_type, 
    geom_point(data=glm_mean, aes(x=mean_temp, y=depth, color="b_gcm_glm_mean"), size=1, alpha=1) +
    scale_y_reverse(lim=c(max(glm_mean$depth),0)) +
    scale_color_manual(values = c("cornflowerblue","midnightblue"), labels = c('GLM preds','mean of GLM preds'), guide= guide_legend(override.aes = list(alpha = c(0.17,1), size=c(0.5,1)))) +
    facet_grid(doy ~ period, labeller=labeller(.cols=label_both)) +
    labs(title= paste(sprintf("%s -- GLM and mean GLM profiles", selected_site), "\n GCM: all", "\n dates: 2/15, 5/15, 8/15, 11/15"), x="Temperature (°C)", y="Depth (m)") +
    theme_bw() +
    theme(
      #             panel.border = element_blank(),
      strip.text = element_text(size=14),
      axis.text = element_text(size=12),
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      legend.text = element_text(size=14),
      legend.title = element_blank(),
      legend.position = "top"
    )
  #     }
  
  # return(doy_plot)
  
  plot_filepath <- sprintf('4_visualize/out/20yr_profiles_%s.png', selected_site)
  ggsave(filename=plot_filepath, plot=doy_plot, dpi=300, width=10, height=6)
}
# plot_20yr_average_profiles(lake_ids[1])
# purrr::map(lake_ids, function(lake_id) {
#   plot_20yr_average_profiles(lake_id)
# })
