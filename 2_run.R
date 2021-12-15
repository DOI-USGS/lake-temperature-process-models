source('2_run/src/run_glm3.R')

p2 <- list(
  # Function will generate file
  # but return a tibble that includes that filename and its hash
  tar_target(
    p2_glm_uncalibrated_runs,
    run_glm3_model(
      sim_dir = '2_run/tmp',
      nml_objs = p1_nml_objects,
      meteo_xwalk = p1_meteo_xwalk,
      export_fl_template = '2_run/tmp/%s_%s_%s.feather'),
    packages = c('retry','glmtools', 'GLM3r'),
    pattern = map(p1_meteo_xwalk)),
  # Group model runs by lake id and gcm
  tar_target(
    p2_glm_uncalibrated_run_groups,
    p2_glm_uncalibrated_runs %>% 
      group_by(lake_id, gcm) %>% 
      tar_group(),
    iteration = "group"
  ),
  # Use grouped target to combine glm output into feather files
  tar_target(
    p2_glm_uncalibrated_output_feathers,{  
      # set filename
      outfile <- sprintf('2_run/out/%s_%s.feather',
                         unique(p2_glm_uncalibrated_run_groups$lake_id),
                         unique(p2_glm_uncalibrated_run_groups$gcm))
      # combine into single feather file and write
      purrr::map_df(p2_glm_uncalibrated_run_groups$export_fl, function(export_file) {
        arrow::read_feather(export_file)
      }) %>% arrow::write_feather(outfile)
      
      return(outfile)},
    format = 'file',
    pattern = map(p2_glm_uncalibrated_run_groups))
)
