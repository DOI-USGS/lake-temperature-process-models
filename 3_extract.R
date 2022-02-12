source('3_extract/src/process_glm_output.R')

p3 <- list(
  # Use grouped target to combine glm output into feather files
  # Function will generate the output feather file for each lake-gcm combo
  tar_target(
    p3_glm_uncalibrated_output_feathers,
    combine_glm_output(p2_glm_uncalibrated_run_groups, 
                       outfile_template='3_extract/out/GLM_%s_%s.feather'),
    format = 'file',
    pattern = map(p2_glm_uncalibrated_run_groups)),
  
  # Generate a tibble with a row for each output file
  # that includes the filename and its hash along with the
  # site_id, gcm, the state the lake is in, and the cell_no 
  # and tile_no for that lake.
  tar_target(
    p3_glm_uncalibrated_output_feather_tibble,
    generate_output_tibble(p2_glm_uncalibrated_run_groups, p3_glm_uncalibrated_output_feathers, p1_lake_cell_tile_xwalk_df),
    pattern = map(p2_glm_uncalibrated_run_groups, p3_glm_uncalibrated_output_feathers)
  ),
  
  # Group output feather tibble by tile number
  tar_target(
    p3_glm_uncalibrated_output_feather_groups,
    p3_glm_uncalibrated_output_feather_tibble %>%
      group_by(tile_no) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Generate a zip file for each tile, zipping the grouped feathers
  tar_target(
    p3_glm_uncalibrated_output_zips,
    zip_output_files(p3_glm_uncalibrated_output_feather_groups,
                     zipfile_template= '3_extract/out/GLM_tile%s.zip'),
    format = 'file',
    pattern = map(p3_glm_uncalibrated_output_feather_groups)
  )
)