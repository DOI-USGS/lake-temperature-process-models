source('2_run/src/run_glm3.R')

p2 <- list(
  # Function will generate file
  # but return a tibble that includes that filename and its hash
  tar_target(
    p2_glm_uncalibrated_runs,
    run_glm3_model(
      sim_dir = '2_run/tmp',
      nml_objs = p1_nml_objects,
      model_config = p1_model_config,
      burn_in = 300,
      burn_out = 190,
      export_fl_template = '2_run/tmp/GLM_%s_%s_%s.feather'),
    packages = c('retry','glmtools', 'GLM3r'),
    pattern = map(p1_model_config)),
  # Group model runs by lake id and gcm
  # Discard the glm diagnostics so they don't trigger rebuilds
  # even when the export_fl_hash is unchanged
  # Filter to only successful model runs within each group
  tar_target(
    p2_glm_uncalibrated_run_groups,
    p2_glm_uncalibrated_runs %>%
      select(site_id, gcm, time_period, raw_meteo_fl, export_fl, export_fl_hash, glm_success) %>%
      group_by(site_id, gcm) %>% 
      filter(all(glm_success)) %>%
      tar_group(),
    iteration = "group"
  )
)
