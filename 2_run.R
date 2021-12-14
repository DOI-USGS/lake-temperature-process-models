source('2_run/src/run_glm3.R')

p2 <- list(
  tar_target(
    p2_glm_uncalibrated,
    run_glm3_model(
      sim_dir = '2_run/out',
      nml_objs = p1_nml_objects,
      meteo_xwalk = p1_meteo_xwalk),
    packages = c('glmtools', 'GLM3r'),
    pattern = map(p1_meteo_xwalk)
  )
  # next, pull output
  # and combine output for each lake, for each gcm, into single feather file
)