source('5_evaluate/src/eval_utility_fxns.R')
source('4_visualize/src/plot_data_utility_fxns.R')

p5 <- list(
  ##### Evaluate GCM model output #####
  
  ##### Evaluate NLDAS model output #####
  
  ###### Prep predictions and observations ######
  
  # Get vector of site_ids for which we have NLDAS output
  tar_target(p3_nldas_export_site_ids,
             p2_nldas_glm_uncalibrated_run_groups %>%
               pull(site_id)),
  
  # Prep site observations
  # filter obs to sites and dates for which we have NLDAS output
  # And further filter obs to those for sites w/ >= 10 dates with observations
  tar_target(p5_obs_for_eval,
             get_eval_obs(p1_obs_feather, p3_nldas_export_site_ids, p1_nldas_dates$driver_start_date, 
                          p1_nldas_dates$driver_end_date, min_obs_dates = 10)),
  
  # Get vector of evaluation sites, based on availability of observations
  tar_target(p5_eval_sites,
             p5_obs_for_eval %>% pull(site_id) %>% unique()),

  # Group filtered obs by site, set up tar_group()
  tar_target(p5_obs_for_eval_groups,
             p5_obs_for_eval %>%
               group_by(site_id) %>%
               tar_group(),
             iteration = "group"),
  
  # Prep NLDAS predictions
  # Read in predictions for sites that are evaluation sites and filter
  # predictions for each site to only those dates that have observations 
  tar_target(
    p5_nldas_preds_eval,
    arrow::read_feather(sprintf('3_extract/out/GLM_%s_NLDAS.feather', p5_eval_sites)) %>%
      filter(time %in% p5_obs_for_eval_groups$time) %>%
      mutate(site_id = p5_eval_sites) %>%
      select(-ice),
    pattern = map(p5_eval_sites, p5_obs_for_eval_groups)
  ),
  
  # Group filtered NLDAS preds by site, set up tar_group()
  tar_target(
    p5_nldas_preds_eval_groups,
    p5_nldas_preds_eval %>%
      group_by(site_id) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Match NLDAS predictions to observations
  # https://github.com/USGS-R/mntoha-data-release/blob/main/src/eval_utils.R#L14-L35
  # actual matching (by site): https://github.com/USGS-R/mntoha-data-release/blob/main/src/eval_utils.R#L113-L132
  # map over eval groups (so parallelizable on Tallgrass)
  tar_target(p5_nldas_pred_obs,
             match_pred_obs(eval_obs = p5_obs_for_eval_groups, eval_preds = p5_nldas_preds_eval_groups) %>%
               select(-tar_group), # drop grouping column
             pattern = map(p5_obs_for_eval_groups, p5_nldas_preds_eval_groups)),

  # Write matched predictions to file
  tar_target(p5_nldas_pred_obs_csv,
             {
               outfile <- '5_evaluate/out/nldas_matched_to_observations.csv'
               readr::write_csv(p5_nldas_pred_obs, outfile)
               return(outfile)
             },
             format = 'file'),
  
  # Prep matched preds for evaluation
  # Add pred_diff column (pred - obs)
  # Filter matched obs-preds w/ pred_diff to surface obs-preds
  # Set up variables for which bias/accuracy will be calculated
  # Add fields for year, doy, season, temp_bin
  tar_target(p5_nldas_pred_obs_surface_eval,
             prep_data_for_eval(p5_nldas_pred_obs, selected_depth = 0)
             ),
  
  ###### Assess model bias ######
  
  # Bias through time - year
  tar_target(p5_nldas_surface_bias_year,
             calc_bias(p5_nldas_pred_obs_surface_eval, grouping_var = 'year')),
  
  # Bias through time - doy
  tar_target(p5_nldas_surface_bias_doy,
             calc_bias(p5_nldas_pred_obs_surface_eval, grouping_var = 'doy')),
  
  # Bias by season
  tar_target(p5_nldas_surface_bias_season,
             calc_bias(p5_nldas_pred_obs_surface_eval, grouping_var = 'season')),

  # Bias for specific temperature ranges
  tar_target(p5_nldas_surface_bias_temp,
             calc_bias(p5_nldas_pred_obs_surface_eval, grouping_var = 'temp_bin')),
  
  ## Plots
  # Bar plot of bias through time, by  year
  tar_target(p5_nldas_surface_bias_year_png,
             plot_evaluation_barplot(p5_nldas_surface_bias_year, driver= 'NLDAS', 
                          y_var = 'bias', x_var = 'year', depth='surface',
                          outfile = '5_evaluate/out/nldas_surface_bias_year.png'),
             format = 'file'),
  
  # Bar plot of bias through time, by doy
  tar_target(p5_nldas_surface_bias_doy_png,
             plot_evaluation_barplot(p5_nldas_surface_bias_doy, driver= 'NLDAS', 
                          y_var = 'bias', x_var = 'doy', depth='surface',
                          outfile = '5_evaluate/out/nldas_surface_bias_doy.png'),
             format = 'file'),
  
  # Bar plot of bias by season
  tar_target(p5_nldas_surface_bias_season_png,
             plot_evaluation_barplot(p5_nldas_surface_bias_season, driver= 'NLDAS', 
                          y_var = 'bias', x_var = 'season', depth='surface',
                          outfile = '5_evaluate/out/nldas_surface_bias_season.png'),
             format = 'file'),
  
  # Bar plot of bias for 2-degree temperature bins
  tar_target(p5_nldas_surface_bias_temp_png,
             plot_evaluation_barplot(p5_nldas_surface_bias_temp, driver= 'NLDAS', 
                          y_var = 'bias', x_var = 'temp_bin', depth='surface',
                          outfile = '5_evaluate/out/nldas_surface_bias_temp.png'),
             format = 'file'),
  
  ###### Assess model accuracy ######
  
  # # Accuracy through time - year
  tar_target(p5_nldas_surface_accuracy_year,
             calc_rmse(p5_nldas_pred_obs_surface_eval, grouping_var = 'year')),
  
  # Accuracy through time - doy
  tar_target(p5_nldas_surface_accuracy_doy,
             calc_rmse(p5_nldas_pred_obs_surface_eval, grouping_var = 'doy')),
  
  # Accuracy by season
  tar_target(p5_nldas_surface_accuracy_season,
             calc_rmse(p5_nldas_pred_obs_surface_eval, grouping_var = 'season')),
  
  # Accuracy for specific temperature ranges
  tar_target(p5_nldas_surface_accuracy_temp,
             calc_rmse(p5_nldas_pred_obs_surface_eval, grouping_var = 'temp_bin')),
  
  ## Plots
  # Bar plot of accuracy through time, by  year
  tar_target(p5_nldas_surface_accuracy_year_png,
             plot_evaluation_barplot(p5_nldas_surface_accuracy_year, driver= 'NLDAS', 
                          y_var = 'rmse', x_var = 'year', depth='surface',
                          outfile = '5_evaluate/out/nldas_surface_accuracy_year.png'),
             format = 'file'),
  
  # Bar plot of accuracy through time, by doy
  tar_target(p5_nldas_surface_accuracy_doy_png,
             plot_evaluation_barplot(p5_nldas_surface_accuracy_doy, driver= 'NLDAS', 
                          y_var = 'rmse', x_var = 'doy', depth='surface',
                          outfile = '5_evaluate/out/nldas_surface_accuracy_doy.png'),
             format = 'file'),
  
  # Bar plot of accuracy by season
  tar_target(p5_nldas_surface_accuracy_season_png,
             plot_evaluation_barplot(p5_nldas_surface_accuracy_season, driver= 'NLDAS', 
                          y_var = 'rmse', x_var = 'season', depth='surface',
                          outfile = '5_evaluate/out/nldas_surface_accuracy_season.png'),
             format = 'file'),
  
  # Bar plot of accuracy for 2-degree temperature bins
  tar_target(p5_nldas_surface_accuracy_temp_png,
             plot_evaluation_barplot(p5_nldas_surface_accuracy_temp, driver= 'NLDAS', 
                          y_var = 'rmse', x_var = 'temp_bin', depth='surface',
                          outfile = '5_evaluate/out/nldas_surface_accuracy_temp.png'),
             format = 'file')
)