library(targets)
library(tidyverse)
library(lubridate)
library(ggplot2)

output_files <- list.files('2_run/out','*.feather')
tar_load(p1_gcm_names)
lake_ids <- c('nhdhr_91597227','nhdhr_72863673','nhdhr_59724783','nhdhr_47719227','nhdhr_45361793',
              'nhdhr_166867643','nhdhr_120020627','nhdhr_120018623','nhdhr_114542327','nhdhr_114539927')
gcm_names <- p1_gcm_names 

model_runs <- tibble(
  output_file = output_files,
  gcm = stringr::str_extract(output_file, paste(gcm_names,collapse="|")),
  lake_id = stringr::str_extract(output_file, paste(lake_ids,collapse="|")))
  
munge_long <- function(input_wide) {
  input_long <- input_wide %>%
    pivot_longer(temp_0:colnames(input_wide)[ncol(input_wide)], names_to="depth", values_to="temperature") %>%
    mutate(depth = as.numeric(str_remove(depth, 'temp_'))) %>%
    arrange(date, depth)
}

get_glm_preds <- function(site, gcm, format='long') {
  glm_preds <- arrow::read_feather(sprintf('2_run/out/GLM_%s_%s.feather', site, gcm)) %>%
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


plot_20yr_preds <- function(model_run) {
  glm_preds <- arrow::read_feather(sprintf('2_run/out/%s',model_run$output_file)) %>%
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
    select(date, year, doy, decade, period, depth_column_surface, depth_column_middle, depth_column_bottom)
  
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
    labs(title= paste(sprintf("%s", model_run$lake_id), sprintf('GCM: %s', model_run$gcm), sep='\n'),  y="Temperature (°C)") +
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
  # return(doy_plot)
  plot_filepath <- sprintf('2_run/out/plots/20yr_preds_ice_%s_%s.png', model_run$lake_id, model_run$gcm)
  ggsave(filename=plot_filepath, plot=doy_plot, dpi=300, width=10, height=6)
}
plot_20yr_preds(test)
purrr::pmap(model_runs, function(...) {
  model_run <- tibble(...) 
  plot_20yr_preds(model_run)
  })




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
  
  plot_filepath <- sprintf('2_run/out/plots/20yr_preds_%s.png', selected_site)
  ggsave(filename=plot_filepath, plot=doy_plot, dpi=300, width=10, height=6)
}

plot_20yr_average_preds(lake_ids[1])
purrr::map(lake_ids, function(lake_id) {
  plot_20yr_average_preds(lake_id)
})

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
  
  plot_filepath <- sprintf('2_run/out/plots/20yr_profiles_%s.png', selected_site)
  ggsave(filename=plot_filepath, plot=doy_plot, dpi=300, width=10, height=6)
}
plot_20yr_average_profiles(lake_ids[1])
purrr::map(lake_ids, function(lake_id) {
  plot_20yr_average_profiles(lake_id)
})

plot_surface_timeseries_ribbon_GCMs <- function(obs_dir, analysis_dir, selected_site, phase, fold, year_list=c(), suffix='') {
  
  # convert array of years to comma-separated character string
  year_list_long <- paste(year_list, collapse=',')
  # within character string, collapse consecutive sequences of indices into ranges 
  year_list_collapsed = collapseConsecutive(year_list_long)
  
  obs <- get_site_obs(obs_dir, selected_site)
  
  glm_preds_access <- get_glm_preds(selected_site, 'access', format='wide')
  glm_preds_gfdl <- get_glm_preds(selected_site, 'gfdl', format='wide')
  glm_preds_cnrm <- get_glm_preds(selected_site, 'cnrm', format='wide')
  glm_preds_ipsl <- get_glm_preds(selected_site, 'ipsl', format='wide')
  glm_preds_mri <- get_glm_preds(selected_site, 'mri', format='wide')
  glm_preds_miroc5 <- get_glm_preds(selected_site, 'miroc5', format='wide')
  
  glm_preds <- bind_rows(glm_preds_access, glm_preds_gfdl, glm_preds_cnrm, glm_preds_ipsl, glm_preds_mri, glm_preds_miroc5)
  
  access_predictions <- get_gcm_pgdl_preds(analysis_dir, selected_site, phase, fold, driver_type='access', format='wide')
  gfdl_predictions <- get_gcm_pgdl_preds(analysis_dir, selected_site, phase, fold, driver_type='gfdl', format='wide')
  cnrm_predictions <- get_gcm_pgdl_preds(analysis_dir, selected_site, phase, fold, driver_type='cnrm', format='wide')
  ipsl_predictions <- get_gcm_pgdl_preds(analysis_dir, selected_site, phase, fold, driver_type='ipsl', format='wide')
  mri_predictions <- get_gcm_pgdl_preds(analysis_dir, selected_site, phase, fold, driver_type='mri', format='wide')
  miroc5_predictions <- get_gcm_pgdl_preds(analysis_dir, selected_site, phase, fold, driver_type='miroc5', format='wide')
  
  dataset <- bind_rows(access_predictions, gfdl_predictions, cnrm_predictions, ipsl_predictions, mri_predictions, miroc5_predictions)
  
  obs_plot <- obs %>%
    filter(format(as.Date(date),'%Y') %in% year_list) %>%
    filter(depth == 0)    
  
  glm_plot <- glm_preds %>%
    filter(format(as.Date(date),'%Y') %in% year_list)
  
  dataset_plot <- dataset %>%
    filter(format(as.Date(date),'%Y') %in% year_list) %>%
    mutate(pos_temp_0 = ifelse(temp_0>=0, temp_0, NA)) %>%
    mutate(neg_temp_0 = ifelse(temp_0<0, temp_0, NA))
  
  options(repr.plot.width=20, repr.plot.height=15)
  
  if (nrow(obs_plot) == 0) {
    area_plot <- dataset_plot %>% ggplot()+
      geom_ribbon(aes(x=doy, ymin= 0, ymax=temp_0*(temp_0>0), fill=alpha('dodgerblue', 0.3))) +
      geom_ribbon(aes(x=doy, ymax=0, ymin=temp_0*(temp_0<0), fill=alpha('red',0.3))) +
      scale_fill_manual(values = c(alpha('dodgerblue', 0.3), alpha('red',0.3)), labels = c('PGDL predictions ≥ 0°C', 'PGDL predictions < 0°C')) +
      geom_line(data=glm_plot, aes(x=doy, y=temp_0, color="black"), size=0.5, linetype='dotted') +
      scale_color_manual(values = c("black"), labels = c('uncalibrated GLM'), guide= guide_legend(override.aes = list(linetype = c('dotted'), shape = c(NA), size=c(0.5)))) +
      facet_grid(year ~ driver_type, labeller=labeller(.cols=label_both)) +
      labs(title= paste(sprintf("%s -- phase: %s", selected_site, phase), '\n depth of predictions: 0m (no obs in selected years)'), y="Surface temperature (°C)") + 
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
  } else {
    area_plot <- dataset_plot %>% ggplot()+
      geom_ribbon(aes(x=doy, ymin= 0, ymax=temp_0*(temp_0>0), fill=alpha('dodgerblue', 0.3))) +
      geom_ribbon(aes(x=doy, ymax=0, ymin=temp_0*(temp_0<0), fill=alpha('red',0.3))) +
      scale_fill_manual(values = c(alpha('dodgerblue', 0.3), alpha('red',0.3)), labels = c('PGDL predictions ≥ 0°C', 'PGDL predictions < 0°C')) +
      geom_line(data=glm_plot, aes(x=doy, y=temp_0, color="black"), size=0.5, linetype='dotted') +
      geom_point(data=obs_plot, aes(x=doy, y=temp, color="green4"), size=2) +
      scale_color_manual(values = c("black", "green4"), labels = c('uncalibrated GLM', 'observations'), guide= guide_legend(override.aes = list(linetype = c('dotted', 'blank'), shape = c(NA, 16), size=c(0.5,2)))) +
      facet_grid(year ~ driver_type, labeller=labeller(.cols=label_both)) +
      labs(title= paste(sprintf("%s -- phase: %s", selected_site, phase), '\n depth of predictions: 0m'), y="Surface temperature (°C)") + 
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
  }
  
  
  return(area_plot)
  
  #     plot_filepath <- sprintf('%s/PGDL_GCM_plots/GCM_grids_%s/%s_depth0_%s_ribbon_%s%s.jpg', analysis_dir, phase, selected_site, phase, year_list_collapsed, suffix)
  #     if(!dir.exists(dirname(plot_filepath))) dir.create(dirname(plot_filepath), recursive = TRUE)
  
  #     ggsave(filename=plot_filepath, plot=area_plot, dpi=300, width=20, height=15)
}

plot_deeper_timeseries_ribbon <- function(obs_dir, analysis_dir, selected_site, driver_type, depth_class, obs_threshold, year_list=c(), supplemental_years = c(), suffix='') {
  
  # read in data files
  obs <- get_site_obs(obs_dir, selected_site)
  glm_preds <- get_glm_preds(selected_site, driver_type, format='wide')
  pretrain_predictions <- get_gcm_pgdl_preds(analysis_dir, selected_site, phase='pretrain', fold='', driver_type, format='wide') %>%
    mutate(phase_plot = '1_pretrain')
  finetune_predictions <- get_gcm_pgdl_preds(analysis_dir, selected_site, phase='finetune', fold='', driver_type, format='wide') %>%
    mutate(phase_plot = '2_finetune')
  
  dataset <- bind_rows(pretrain_predictions, finetune_predictions)
  
  # figure out which depths are well represented in the observation dataset for the selected site in the selected years
  # filter to year < 2001 b/c of GCM contemporary period
  print(paste('The selected site is:', selected_site))
  obs_subset <- obs %>%
    filter(year < 2001 & year > 1980)
  
  n_lambda <- 1
  
  depth_scores <- obs_subset %>% group_by(depth) %>% tally() %>%
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
  
  print(paste('The selected depth class is:', depth_class))
  plot_depth = unname(depths[depth_class])
  
  # subset dataset to plot depth
  obs_subset_depth <- obs_subset %>%
    filter(depth == plot_depth)
  
  print(paste('There are', nrow(obs_subset_depth), 'observations at that depth'))
  
  # if year_list is not specified
  if (length(year_list) == 0) {
    print(paste('Generating a list of years with >', obs_threshold, ' observations'))
    # get count of obs at that depth in each year
    year_count <- obs_subset_depth %>%
      group_by(year) %>%
      tally()
    
    # make list of years w/ obs count > obs_threshold
    year_list = c()
    for (i in 1:nrow(year_count)) {if (year_count[i,'n'] > obs_threshold) year_list <- append(year_list, year_count[i,'year']$year)}
    
    # if only 1 year with 1 obs, set that year as year_list (otherwise hit error)
    if (is.null(year_list)) print(paste('There are no years with more than', obs_threshold, 'observations'))
  } else {
    print('The year list has been specified')
  }
  
  # add supplemental years
  year_list <- c(year_list, supplemental_years)
  
  # convert array of years to comma-separated character string
  year_list_long <- paste(year_list, collapse=',')
  
  # if multiple years in year list, collapse to ranges if possible
  if (length(year_list) > 1) {
    # within character string, collapse consecutive sequences of indices into ranges 
    year_list_collapsed <- collapseConsecutive(year_list_long)
  } else {
    year_list_collapsed <- year_list_long
  }
  
  print(paste('The selected years are:', year_list_long))
  
  # subset data futher to only include data from year list
  obs_plot <- obs_subset_depth %>%
    filter(format(as.Date(date),'%Y') %in% year_list)
  
  #     # If no observations in that year
  #     # print warning message if there are no observations in the specified year
  #     # (should only occur in cases where user specified year_list)
  #     if (nrow(obs_plot) == 0) {
  #         print(paste('There are', nrow(obs_plot), 'observations at that depth in the specified year(s). Please specify a different year'))
  #         return(message("Cannot generate plot"))
  #     } else {
  #         print(paste('There are', nrow(obs_plot), 'observations at that depth in the selected year(s)'))
  
  # subset the glm uncalibrated predictions dataset
  glm_subset <- glm_preds %>%
    filter(format(as.Date(date),'%Y') %in% year_list)
  
  # subset the pgdl predictions dataset
  dataset_subset <- dataset %>%
    filter(format(as.Date(date),'%Y') %in% year_list)
  
  # for the glm and pgdl predictions dataframes, identify which temperature column to pull
  # round down to nearest .5
  plot_depth <- plot_depth - (plot_depth * 10) %% 5 / 10
  
  # get column name
  dataset_depth = plot_depth
  depth_column = paste0('temp_',dataset_depth)
  
  print(paste('Depth class:', depth_class, ', Depth: ', plot_depth))
  
  if (depth_column %in% colnames(glm_subset) == FALSE | depth_column %in% colnames(dataset_subset) == FALSE) {
    print(paste(depth_column,'is not a column in the GLM or PGDL dataset'))
    while (TRUE){
      dataset_depth = dataset_depth - 1
      depth_column = paste0('temp_', dataset_depth)
      print(paste('Checking datasets for new depth column: ', depth_column))
      if (depth_column %in% colnames(glm_subset) == TRUE & depth_column %in% colnames(dataset_subset) == TRUE) {
        break
      } else {
        next
      }
    }
  }
  print(paste('Closest temperature column in prediction datasets:', depth_column))
  
  # modify the glm uncalibrated predictions dataset for plotting
  glm_plot <- glm_subset %>%
    select(date, depth_column, year, doy)
  
  # modify the pgdl predictions dataset for plotting
  dataset_plot <- dataset_subset %>%
    select(date, depth_column, phase, phase_plot, year, doy)  
  
  plot_height = if (length(year_list) > 10) 30 else 15
  options(repr.plot.width=10, repr.plot.height=plot_height)
  
  if (nrow(obs_plot) == 0) {
    area_plot <- dataset_plot %>% ggplot()+
      geom_ribbon(aes(x=doy, ymin= 0, ymax=get(depth_column)*(get(depth_column)>0), fill=alpha('dodgerblue', 0.3))) +
      geom_ribbon(aes(x=doy, ymax=0, ymin=get(depth_column)*(get(depth_column)<0), fill=alpha('red',0.3))) +
      scale_fill_manual(values = c(alpha('dodgerblue', 0.3), alpha('red',0.3)), labels = c('PGDL predictions ≥ 0°C', 'PGDL predictions < 0°C')) +
      #         geom_line(aes(x=doy, y=get(depth_column), color="purple"), size=0.8) +
      geom_line(data=glm_plot, aes(x=doy, y=get(depth_column), color="black"), size=0.6, linetype='dotted') +
      #         geom_point(data=obs_plot, aes(x=doy, y=temp, color="green4"), size=2) +
      scale_color_manual(values = c("black"), labels = c('uncalibrated GLM'), guide= guide_legend(override.aes = list(linetype = c('dotted'), shape = c(NA), size=c(0.6)))) +
      #         scale_color_manual(values = c("black","green4"), labels = c('uncalibrated GLM','observations'), guide= guide_legend(override.aes = list(linetype = c('dotted','blank'), shape = c(NA, 16), size=c(0.6,2)))) +
      #         scale_color_manual(values = c("black","green4","purple"), labels = c('uncalibrated GLM','observations','PGDL'), guide= guide_legend(override.aes = list(linetype = c('dotted','blank', 'solid'), shape = c(NA, 16, NA), size=c(0.6,2,0.8)))) +
      facet_grid(year~phase_plot, labeller=labeller(.cols=label_both)) +
      labs(title= paste(sprintf("%s -- driver: %s", selected_site, driver_type), sprintf("\n No obs in selected years, GLM and PGDL column: %s", plot_depth, depth_column)), y="Temperature (°C)") + 
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
  } else {
    area_plot <- dataset_plot %>% ggplot()+
      geom_ribbon(aes(x=doy, ymin= 0, ymax=get(depth_column)*(get(depth_column)>0), fill=alpha('dodgerblue', 0.3))) +
      geom_ribbon(aes(x=doy, ymax=0, ymin=get(depth_column)*(get(depth_column)<0), fill=alpha('red',0.3))) +
      scale_fill_manual(values = c(alpha('dodgerblue', 0.3), alpha('red',0.3)), labels = c('PGDL predictions ≥ 0°C', 'PGDL predictions < 0°C')) +
      #         geom_line(aes(x=doy, y=get(depth_column), color="purple"), size=0.8) +
      geom_line(data=glm_plot, aes(x=doy, y=get(depth_column), color="black"), size=0.6, linetype='dotted') +
      geom_point(data=obs_plot, aes(x=doy, y=temp, color="green4"), size=2) +
      scale_color_manual(values = c("black","green4"), labels = c('uncalibrated GLM','observations'), guide= guide_legend(override.aes = list(linetype = c('dotted','blank'), shape = c(NA, 16), size=c(0.6,2)))) +
      #         scale_color_manual(values = c("black","green4","purple"), labels = c('uncalibrated GLM','observations','PGDL'), guide= guide_legend(override.aes = list(linetype = c('dotted','blank', 'solid'), shape = c(NA, 16, NA), size=c(0.6,2,0.8)))) +
      facet_grid(year~phase_plot, labeller=labeller(.cols=label_both)) +
      labs(title= paste(sprintf("%s -- driver: %s", selected_site, driver_type), sprintf("\n Depth of observations: %s, GLM and PGDL column: %s", plot_depth, depth_column)), y="Temperature (°C)") + 
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
  }
  
  return(area_plot)
  
  #     plot_filepath <- sprintf('%s/PGDL_GCM_plots/GCM_specific_pretrain_vs_finetune_varying_depths/%s/%s/%s_depth%s_%s_ribbon_%s%s.jpg', analysis_dir, depth_class, driver_type, selected_site, plot_depth, driver_type, year_list_collapsed, suffix)
  #     if(!dir.exists(dirname(plot_filepath))) dir.create(dirname(plot_filepath), recursive=TRUE)
  
  #     ggsave(filename=plot_filepath, plot=area_plot, dpi=300, width=10, height=plot_height)
}

collapseConsecutive <- function(s){
  x <- as.numeric(unlist(strsplit(s, ",")))
  
  x_0 <- x[1]
  out <- toString(x[1])
  hasDash <- FALSE
  
  for(i in 2:length(x)) {
    x_1 <- x[i]
    x_2 <- x[i+1]
    
    if((x_0 + 1) == x_1 && !is.na(x_2) && (x_1 + 1) == x_2) {
      if(!hasDash) {
        out <- c(out, "-")
        hasDash <- TRUE
      }
    } else {
      if(hasDash) {
        hasDash <- FALSE
      } else {
        out <- c(out, ",")
      }
      out <- c(out, x_1)
      hasDash <- FALSE
    }
    x_0 <- x_1
  }
  outString <- paste(out, collapse="")
  outString
}
