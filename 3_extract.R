source('3_extract/src/process_glm_output.R')

p3 <- list(
  # Use grouped target to combine glm output into feather files
  # Function will generate the output feather file for each lake-gcm combo
  # but return a tibble that includes the filename and its hash
  tar_target(
    p3_glm_uncalibrated_output_feathers,
    combine_glm_output(p2_glm_uncalibrated_run_groups, p1_lake_cell_tile_xwalk_df, 
                       outfile_template='3_extract/out/GLM_%s_%s.feather'),
    pattern = map(p2_glm_uncalibrated_run_groups)),
  
  # Group output feathers by spatial tile number
  tar_target(
    p3_glm_uncalibrated_output_feather_groups,
    p3_glm_uncalibrated_output_feathers %>%
      group_by(spatial_tile_no) %>%
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