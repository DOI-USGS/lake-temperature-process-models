source('5_evaluate/src/eval_utility_fxns.R')
source('4_visualize/src/plot_data_utility_fxns.R')

p5 <- list(
  ##### Evaluate GCM model output #####
  
  ##### Evaluate NLDAS model output #####
  
  ###### Prep predictions and observations ######
  
  # Get vector of site_ids for which we have NLDAS output
  # For now, evaluate only predictions for lakes within CASC states
  tar_target(p5_CASC_states,
             c('ND','SD','IA','MI','IN','IL','WI','MN','MO','AR','OH')),
  tar_target(p5_nldas_export_site_ids,
             p3_nldas_glm_uncalibrated_output_feather_tibble %>%
               filter(state %in% p5_CASC_states) %>%
               arrange(site_id) %>%
               pull(site_id)),
  
  # Prep site observations
  # filter obs to sites and dates for which we have NLDAS output
  # And further filter obs to those for sites w/ >= `min_obs_dates` dates with observations
  tar_target(p5_obs_for_eval,
             get_eval_obs(p1_obs_feather, p5_nldas_export_site_ids, p1_nldas_dates$driver_start_date, 
                          p1_nldas_dates$driver_end_date, min_obs_dates = 10)),

  # Group filtered obs by site, set up tar_group()
  tar_target(p5_obs_for_eval_groups,
             p5_obs_for_eval %>%
               group_by(site_id) %>%
               tar_group(),
             iteration = "group"),
  
  # Match NLDAS predictions to observations
  # map over obs_for_eval_groups (so parallelizable on Tallgrass)
  tar_target(p5_nldas_pred_obs,
             match_pred_obs(preds_file = sprintf('3_extract/out/GLM_%s_NLDAS.feather', unique(p5_obs_for_eval_groups$site_id)),
                            eval_obs = p5_obs_for_eval_groups),
             pattern = map(p5_obs_for_eval_groups)),

  # Write matched pred-obs to file
  tar_target(p5_nldas_pred_obs_csv,
             {
               outfile <- '5_evaluate/out/nldas_matched_to_observations.csv'
               readr::write_csv(p5_nldas_pred_obs, outfile)
               return(outfile)
             },
             format = 'file'),
  
  # Get vector of evaluation sites, based on matched pred-obs
  tar_target(p5_eval_sites,
             p5_nldas_pred_obs %>% pull(site_id) %>% unique()),
  
  # Pull out lake depth for each evaluation site
  tar_target(
    p5_nldas_depths,
    purrr::map_df(p5_eval_sites, function(site_id) {
      site_nml <- p1_nldas_nml_list_subset[[site_id]]
      tibble(
        site_id = site_id,
        lake_depth = site_nml$lake_depth
      )
    })
  ),
  
  # Prep matched preds for evaluation
  # Add pred_diff column (pred - obs)
  # Set up variables for which bias/accuracy will be calculated
  # Add fields for year, depth_class, doy, doy_bin, season, temp_bin
  tar_target(p5_nldas_pred_obs_eval,
             prep_data_for_eval(p5_nldas_pred_obs, p5_nldas_depths, surface_max_depth = 1, 
                                bottom_depth_factor = 0.85, doy_bin_size = 5, temp_bin_size = 2)),
  
  # Filter evaluation pred-obs to surface and bottom predictions and set up for group mapping
  tar_target(p5_nldas_pred_obs_eval_groups,
             p5_nldas_pred_obs_eval %>%
               filter(depth_class %in% c('surface','bottom')) %>%
               group_by(depth_class) %>%
               tar_group(),
             iteration = "group"),
  
  ###### Assess model bias ######
  
  # Bias through time - year
  tar_target(p5_nldas_bias_year,
             calc_bias(p5_nldas_pred_obs_eval_groups, grouping_var = 'year',
                       depth_class = unique(p5_nldas_pred_obs_eval_groups$depth_class)),
             pattern = map(p5_nldas_pred_obs_eval_groups)),
  
  # Bias through time - doy
  tar_target(p5_nldas_bias_doy,
             calc_bias(p5_nldas_pred_obs_eval_groups, grouping_var = 'doy_bin',
                       depth_class = unique(p5_nldas_pred_obs_eval_groups$depth_class)),
             pattern = map(p5_nldas_pred_obs_eval_groups)),
  
  # Bias by season
  tar_target(p5_nldas_bias_season,
             calc_bias(p5_nldas_pred_obs_eval_groups, grouping_var = 'season',
                       depth_class = unique(p5_nldas_pred_obs_eval_groups$depth_class)),
             pattern = map(p5_nldas_pred_obs_eval_groups)),

  # Bias for specific temperature ranges
  tar_target(p5_nldas_bias_temp,
             calc_bias(p5_nldas_pred_obs_eval_groups, grouping_var = 'temp_bin',
                       depth_class = unique(p5_nldas_pred_obs_eval_groups$depth_class)),
             pattern = map(p5_nldas_pred_obs_eval_groups)),
  
  ## Plots
  # Bar plot of bias through time, by  year
  tar_target(p5_nldas_bias_year_png,
             plot_evaluation_barplot(p5_nldas_bias_year, num_eval_sites = length(p5_eval_sites), 
                                     driver= 'NLDAS', y_var = 'bias', y_label = 'predicted - observed', 
                                     x_var = 'year', faceting_variable = 'depth_class',
                                     outfile = '5_evaluate/out/nldas_bias_year.png'),
             format = 'file'),
  
  # Bar plot of bias through time, by doy
  tar_target(p5_nldas_bias_doy_png,
             plot_evaluation_barplot(p5_nldas_bias_doy,  num_eval_sites = length(p5_eval_sites), 
                                     driver= 'NLDAS', y_var = 'bias', y_label = 'predicted - observed', 
                                     x_var = 'doy_bin', faceting_variable = 'depth_class',
                                     outfile = '5_evaluate/out/nldas_bias_doy.png',
                                     plot_width = 12),
             format = 'file'),
  
  # Bar plot of bias by season
  tar_target(p5_nldas_bias_season_png,
             plot_evaluation_barplot(p5_nldas_bias_season, num_eval_sites = length(p5_eval_sites), 
                                     driver= 'NLDAS', y_var = 'bias', y_label = 'predicted - observed', 
                                     x_var = 'season', faceting_variable = 'depth_class',
                                     outfile = '5_evaluate/out/nldas_bias_season.png',
                                     plot_width = 6),
             format = 'file'),
  
  # Bar plot of bias for 2-degree temperature bins
  tar_target(p5_nldas_bias_temp_png,
             plot_evaluation_barplot(p5_nldas_bias_temp, num_eval_sites = length(p5_eval_sites),
                                     driver= 'NLDAS', y_var = 'bias', y_label = 'predicted - observed', 
                                     x_var = 'temp_bin', faceting_variable = 'depth_class',
                                     outfile = '5_evaluate/out/nldas_bias_temp.png'),
             format = 'file'),
  
  ###### Assess model accuracy ######
  
  # # Accuracy through time - year
  tar_target(p5_nldas_accuracy_year,
             calc_rmse(p5_nldas_pred_obs_eval_groups, grouping_var = 'year',
                       depth_class = unique(p5_nldas_pred_obs_eval_groups$depth_class)),
             pattern = map(p5_nldas_pred_obs_eval_groups)),
  
  # Accuracy through time - doy
  tar_target(p5_nldas_accuracy_doy,
             calc_rmse(p5_nldas_pred_obs_eval_groups, grouping_var = 'doy_bin',
                       depth_class = unique(p5_nldas_pred_obs_eval_groups$depth_class)),
             pattern = map(p5_nldas_pred_obs_eval_groups)),
  
  # Accuracy by season
  tar_target(p5_nldas_accuracy_season,
             calc_rmse(p5_nldas_pred_obs_eval_groups, grouping_var = 'season',
                       depth_class = unique(p5_nldas_pred_obs_eval_groups$depth_class)),
             pattern = map(p5_nldas_pred_obs_eval_groups)),
  
  # Accuracy for specific temperature ranges
  tar_target(p5_nldas_accuracy_temp,
             calc_rmse(p5_nldas_pred_obs_eval_groups, grouping_var = 'temp_bin',
                       depth_class = unique(p5_nldas_pred_obs_eval_groups$depth_class)),
             pattern = map(p5_nldas_pred_obs_eval_groups)),
  
  ## Plots
  # Bar plot of accuracy through time, by  year
  tar_target(p5_nldas_accuracy_year_png,
             plot_evaluation_barplot(p5_nldas_accuracy_year, num_eval_sites = length(p5_eval_sites),
                                     driver= 'NLDAS', y_var = 'rmse', y_label = 'rmse', 
                                     x_var = 'year', faceting_variable = 'depth_class',
                                     outfile = '5_evaluate/out/nldas_accuracy_year.png'),
             format = 'file'),
  
  # Bar plot of accuracy through time, by doy
  tar_target(p5_nldas_accuracy_doy_png,
             plot_evaluation_barplot(p5_nldas_accuracy_doy, num_eval_sites = length(p5_eval_sites),
                                     driver= 'NLDAS', y_var = 'rmse', y_label = 'rmse', 
                                     x_var = 'doy_bin', faceting_variable = 'depth_class',
                                     outfile = '5_evaluate/out/nldas_accuracy_doy.png',
                                     plot_width = 12),
             format = 'file'),
  
  # Bar plot of accuracy by season
  tar_target(p5_nldas_accuracy_season_png,
             plot_evaluation_barplot(p5_nldas_accuracy_season, num_eval_sites = length(p5_eval_sites),
                                     driver= 'NLDAS', y_var = 'rmse', y_label = 'rmse', 
                                     x_var = 'season', faceting_variable = 'depth_class',
                                     outfile = '5_evaluate/out/nldas_accuracy_season.png',
                                     plot_width = 6),
             format = 'file'),
  
  # Bar plot of accuracy for 2-degree temperature bins
  tar_target(p5_nldas_accuracy_temp_png,
             plot_evaluation_barplot(p5_nldas_accuracy_temp, num_eval_sites = length(p5_eval_sites),
                                     driver= 'NLDAS', y_var = 'rmse', y_label = 'rmse', 
                                     x_var = 'temp_bin', faceting_variable = 'depth_class',
                                     outfile = '5_evaluate/out/nldas_accuracy_temp.png'),
             format = 'file')
)