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
      burn_in = 10, #300,
      burn_out = 10, #190,
      export_fl_template = '2_run/tmp/GLM_%s_%s_%s.feather'),
    packages = c('retry','glmtools', 'GLM3r'),
    pattern = map(p1_model_config)),
  # Group model runs by lake id and gcm
  # Discard the glm diagnostics so they don't trigger rebuilds
  # even when the export_fl_hash is unchanged
  tar_target(
    p2_glm_uncalibrated_run_groups,
    p2_glm_uncalibrated_runs %>%
      select(lake_id, gcm, time_period, export_fl, export_fl_hash) %>%
      group_by(lake_id, gcm) %>% 
      tar_group(),
    iteration = "group"
  ),
  # Use grouped target to combine glm output into feather files
  # Use error = 'continue' to keep building if a branch fails
  tar_target(
    p2_glm_uncalibrated_output_feathers,{  
      # set filename
      outfile <- sprintf('2_run/out/GLM_%s_%s.feather',
                         unique(p2_glm_uncalibrated_run_groups$lake_id),
                         unique(p2_glm_uncalibrated_run_groups$gcm))
      # combine into single feather file and write
      purrr::map_df(p2_glm_uncalibrated_run_groups$export_fl, function(export_file) {
        arrow::read_feather(export_file)
      }) %>% arrow::write_feather(outfile)
      
      return(outfile)},
    format = 'file',
    pattern = map(p2_glm_uncalibrated_run_groups),
    error = 'continue')
)
