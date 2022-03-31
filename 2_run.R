source('2_run/src/run_glm3.R')

p2 <- list(
  # Function will generate file
  # but return a tibble that includes that filename and its hash
  # put simulation directories in sub-directory for easy deletion
  tar_target(
    p2_glm_uncalibrated_runs,
    {
      # check mapping to ensure is correct
      tar_assert_identical(p1_model_config$site_id, p1_nml_objects$morphometry$lake_name, "p1_nml_object site id doesn't match p1_model_config site id")
      run_glm3_model(
        sim_dir = '2_run/tmp/simulations',
        nml_obj = p1_nml_objects,
        model_config = p1_model_config,
        export_fl_template = '2_run/tmp/GLM_%s_%s_%s.feather')
    },
    packages = c('retry','glmtools', 'GLM3r'),
    pattern = map(p1_model_config, cross(p1_nml_objects, p1_gcm_names, p1_gcm_dates))),
  
  # For bundling of results by lake and by GCM in 3_extract
  # Group model runs by lake id and gcm
  # Discard the glm diagnostics so they don't trigger rebuilds
  # even when the export_fl_hash is unchanged
  # Filter to groups where all model runs were successful 
  # (if any failed, the group is filtered out)
  tar_target(
    p2_glm_uncalibrated_run_groups,
    p2_glm_uncalibrated_runs %>%
      select(site_id, gcm, time_period, gcm_start_date, gcm_end_date, 
             export_fl, export_fl_hash, glm_success) %>%
      group_by(site_id, gcm) %>% 
      filter(all(glm_success)) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # For bundling of results by lake in 4_visualize
  # Group model runs by lake id only
  # Discard the glm diagnostics so they don't trigger rebuilds
  # even when the export_fl_hash is unchanged
  # Filter to groups where all model runs were successful 
  # (if any failed, the group is filtered out)
  tar_target(
    p2_glm_uncalibrated_lake_groups,
    p2_glm_uncalibrated_runs %>%
      select(site_id, gcm, time_period, raw_meteo_fl, export_fl, export_fl_hash, glm_success) %>%
      group_by(site_id) %>% 
      filter(all(glm_success)) %>%
      tar_group(),
    iteration = "group"
  )
)
