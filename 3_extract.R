source('3_extract/src/process_glm_output.R')

p3 <- list(
  # Use grouped target to combine glm output into feather files
  # Use error = 'continue' to keep building if a branch fails
  # Function will generate file
  # but return a tibble that includes that filename and its hash
  tar_target(
    p3_glm_uncalibrated_output_feathers,
    combine_glm_output(p2_glm_uncalibrated_run_groups, p1_lake_cell_tile_xwalk_df, outfile_template='3_extract/out/GLM_%s_%s.feather'),
    pattern = map(p2_glm_uncalibrated_run_groups)),
  
  # Group output feathers by tile number
  tar_target(
    p3_glm_uncalibrated_output_feather_groups,
    p3_glm_uncalibrated_output_feathers %>%
      group_by(tile_no) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Generate a zip file of output feathers
  # for each tile
  tar_target(
    p3_glm_uncalibrated_output_zips,
    {
      files_to_zip <- p3_glm_uncalibrated_output_feather_groups$export_fl
      zipfile_out <- sprintf('3_extract/out/GLM_tile%s.zip', 
                          unique(p3_glm_uncalibrated_output_feather_groups$tile_no))
      # In order to use `zip`, you need to be in the same working directory as
      # the files you want to zip up.
      project_dir <- getwd()
      setwd(unique(dirname(files_to_zip)))
      zip::zip(file.path(project_dir, zipfile_out), basename(files_to_zip))
      setwd(project_dir)
      return(zipfile_out)
    },
    format = 'file',
    pattern = map(p3_glm_uncalibrated_output_feather_groups)
  )
)