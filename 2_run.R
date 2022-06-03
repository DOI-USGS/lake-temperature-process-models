source('2_run/src/run_glm3.R')

p2 <- list(
  
  ##### GCM model runs #####
  # Function will generate file
  # but return a tibble that includes that filename and its hash
  # puts simulation directories in sub-directory for easy deletion
  # Maps over the grouped p1_gcm_model_config_groups, which has 
  # `p1_gcm_group_length` (18) model runs per group, or 1 group 
  # per lake (6 GCMS * 3 time periods = 18)
  tar_target(
    p2_gcm_glm_uncalibrated_runs,
    {
      # check mapping to ensure is correct
      tar_assert_identical(unique(p1_gcm_model_config_groups$site_id), p1_gcm_nml_objects$morphometry$lake_name, "p1_nml_object site id doesn't match unique p1_gcm_model_config_groups site id")
      purrr::pmap_dfr(p1_gcm_model_config_groups, function(...) {
        run_config <- tibble(...)
        run_glm3_model(
          sim_dir = '2_run/tmp/simulations',
          nml_obj = p1_gcm_nml_objects,
          model_config = run_config,
          export_fl_template = '2_run/tmp/GLM_%s_%s_%s.feather')
      })
    },
    packages = c('retry','glmtools', 'GLM3r'),
    pattern = map(p1_gcm_model_config_groups, p1_gcm_nml_objects)), # W/ all runs for each lake in 1 config group, can map over nml objects w/o cross(), as is same length as # of config groups
  
  # For bundling of results in 3_extract
  # Discard the glm diagnostics so they don't trigger rebuilds
  # even when the export_fl_hash is unchanged
  # Group model runs by lake id
  # Filter to groups where all model runs were successful 
  # (if any failed, the group is filtered out)
  tar_target(
    p2_gcm_glm_uncalibrated_run_groups,
    p2_gcm_glm_uncalibrated_runs %>%
      select(site_id, driver, time_period, driver_start_date, driver_end_date, 
             export_fl, export_fl_hash, glm_success) %>%
      arrange(site_id, driver) %>% # arrange by driver so that GCMs in alphabetical order
      group_by(site_id) %>% 
      filter(all(glm_success)) %>% # Check that all 18 models (3 time periods * 6 GCMs) succeeded for each lake
      tar_group(),
    iteration = "group"
  ),
  
  ##### NLDAS model runs #####
  # Function will generate file
  # but return a tibble that includes that filename and its hash
  # put simulation directories in sub-directory for easy deletion
  tar_target(
    p2_nldas_glm_uncalibrated_runs,
    {
      # check mapping to ensure is correct
      tar_assert_identical(p1_nldas_model_config$site_id, p1_nldas_nml_objects$morphometry$lake_name, "p1_nldas_nml_objects site id doesn't match p1_nldas_model_config site id")
      run_glm3_model(
        sim_dir = '2_run/tmp/nldas_simulations',
        nml_obj = p1_nldas_nml_objects,
        model_config = p1_nldas_model_config,
        export_fl_template = '2_run/tmp/GLM_%s_%s_%s.feather')
    },
    packages = c('retry','glmtools', 'GLM3r'),
    pattern = map(p1_nldas_model_config, p1_nldas_nml_objects)),
  
  # For bundling of results by lake in 3_extract
  # Discard the glm diagnostics so they don't trigger rebuilds
  # even when the export_fl_hash is unchanged
  # Group model runs by lake id
  # Filter to groups where all model runs were successful 
  # (if any failed, the group is filtered out)
  tar_target(
    p2_nldas_glm_uncalibrated_run_groups,
    p2_nldas_glm_uncalibrated_runs %>%
      select(site_id, driver, time_period, driver_start_date, driver_end_date, 
             export_fl, export_fl_hash, glm_success) %>% 
      filter(glm_success) %>%
      group_by(site_id) %>% # Group by site_id for creating export files
      tar_group(),
    iteration = "group"
  )
)
