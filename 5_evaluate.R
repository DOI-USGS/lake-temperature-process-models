source('5_evaluate/src/eval_utility_fxns.R')
source('4_visualize/src/plot_data_utility_fxns.R')

p5 <- list(
  ##### Evaluate GCM model output #####
  
  ##### Evaluate NLDAS model output #####
  
  ### Prep predictions and observations
  
  # Get NLDAS preds in long format
  # As-is, model results are read in by `write_glm_output()` in the `3_extract` step
  # We could modify that function and split the `p3_nldas_glm_uncalibrated_output_feathers`
  # target into two - one that reads in the output in wide format - `p3_nldas_glm_uncalibrated_output`, 
  # and a second that writes it to a feather file - `p3_nldas_glm_uncalibrated_output_feathers`
  # then we could use that first target here to get NLDAS in long format, per https://github.com/USGS-R/mntoha-data-release/blob/main/src/eval_utils.R#L23-L30
  tar_target(
    p5_nldas_preds_long,
    munge_long(p3_nldas_glm_uncalibrated_output) %>%
      mutate(site_id = p2_nldas_glm_uncalibrated_run_groups$site_id, .before=1) %>%
      select(-ice),
    pattern = map(p3_nldas_glm_uncalibrated_output, p2_nldas_glm_uncalibrated_run_groups)
  ),
  
  # load site observations
  # Bring in `7b_temp_merge/out/merged_temp_data_daily.feather` from `lake-temperature-model-prep`
  # (would be target in 1_prep.R)
  tar_target(p5_obs),
  
  # Filter obs to sites for which we have NLDAS output
  # Use https://github.com/USGS-R/mntoha-data-release/blob/main/src/file_utils.R#L441-L446
  # Passing `driver_start_date` and `driver_end_date` from `p1_nldas_dates` 
  tar_target(p5_filtered_obs),
  
  # Subset obs to those for sites w/ >= 10 dates with observations
  # Use https://github.com/USGS-R/mntoha-data-release/blob/main/src/eval_utils.R#L4-L11
  tar_target(p5_obs_for_eval,
             filter_min_dates(p5_filtered_obs, min_dates = 10)),
  
  # Get vector of evaluation sites
  tar_target(p5_eval_sites,
             p5_obs_for_eval %>% pull(site_id) %>% unique()),
  
  # Group subsetted obs by site, set up tar_group()
  tar_target(p5_obs_for_eval_groups,
             p5_obs_for_eval %>%
               group_by(site_id) %>%
               tar_group(),
             iteration = "group"),
  
  # filter long-format NLDAS preds to only evaluation sites
  tar_target(
    p5_nldas_preds_eval,
    p5_nldas_preds_long %>%
      filter(site_id %in% p5_eval_sites)
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
             match_pred_obs(eval_obs = p5_obs_for_eval_groups, eval_preds = p5_nldas_preds_eval_groups),
             pattern = map(p5_obs_for_eval_groups, p5_nldas_preds_eval_groups)),
  
  # Write matched predictions to file
  tar_target(p5_nldas_pred_obs_csv),
  
  # Add pred_diff column (pred - obs) to matched obs-preds
  tar_target(p5_nldas_pred_obs_diff),
  
  # Filter matched obs-preds w/ pred_diff to surface obs-preds
  tar_target(p5_nldas_pred_obs_diff_surface),
  
  # Set up variables for which bias/accuracy will be calculated
  tar_target(p5_nldas_pred_obs_diff_surface_vars,
             get_grouping_vars(p5_nldas_pred_obs_diff_surface) # Add fields for year, doy, season, temp_bin
             ),
  
  ### BIAS TARGETS
  
  # Bias through time - year
  tar_target(p5_nldas_surface_bias_year,
             calc_bias(p5_nldas_pred_obs_diff_surface_vars, grouping_var = 'year')),
  
  # Bias through time - doy
  tar_target(p5_nldas_surface_bias_doy,
             calc_bias(p5_nldas_pred_obs_diff_surface_vars, grouping_var = 'doy')),
  
  # Bias by season
  tar_target(p5_nldas_surface_bias_season,
             calc_bias(p5_nldas_pred_obs_diff_surface_vars, grouping_var = 'season')),
  
  # Bias for specific temperature ranges
  tar_target(p5_nldas_surface_bias_temp,
             calc_bias(p5_nldas_pred_obs_diff_surface_vars, grouping_var = 'temp_bin')),
  
  ## Plots
  # Bar plot of bias through time, by  year
  tar_target(p5_nldas_surface_bias_year_png,
             plot_barplot(p5_nldas_surface_bias_year, y_var = 'bias', 
                          x_var = 'year', depth='surface', 
                          outfile = '5_evaluation/out/nldas_surface_bias_year.png'),
             format = 'file'),
  
  # Bar plot of bias through time, by doy
  
  # Bar plot of bias by season
  
  # Bar plot of bias for 2-degree temperature bins
  
  ### ACCURACY targets
  
  # Accuracy through time - year
  tar_target(p5_nldas_surface_accuracy_year,
             calc_rmse(p5_nldas_pred_obs_diff_surface_vars, grouping_var = 'year')),
  
  # Accuracy through time - doy
  
  # Accuracy by season
  
  # Accuracy for specific temperature ranges
  
  
  ## Plots
  # Bar plot of accuracy through time, by  year
  
  # Bar plot of accuracy through time, by doy
  
  # Bar plot of accuracy by season
  
  # Bar plot of accuracy for 2-degree temperature bins
  
)