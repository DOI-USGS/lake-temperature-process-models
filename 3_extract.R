source('3_extract/src/process_glm_output.R')

p3 <- list(
  ##### Extract GCM model output #####
  # Use grouped runs target to combine GCM glm output into feather files
  # Function will generate the output feather file for each lake-gcm combo
  # across all three time periods, truncating the output to the valid dates 
  # for each time period (excluding the burn-in and burn-out periods) and
  # saving only the temperature predictions for each depth and ice flags
  tar_target(
    p3_gcm_glm_uncalibrated_output_feathers,
    write_glm_output(p2_gcm_glm_uncalibrated_run_groups,
                     outfile_template='3_extract/out/GLM_%s_%s.feather'),
    format = 'file',
    pattern = map(p2_gcm_glm_uncalibrated_run_groups)),
  
  # Generate a tibble with a row for each output file
  # that includes the filename and its hash along with the
  # site_id, driver (gcm name), the state the lake is in,  
  # and the cell_no and tile_no for that lake.
  tar_target(
    p3_gcm_glm_uncalibrated_output_feather_tibble,
    generate_output_tibble(p2_gcm_glm_uncalibrated_run_groups, p3_gcm_glm_uncalibrated_output_feathers, p1_lake_cell_tile_xwalk_df),
    pattern = map(p2_gcm_glm_uncalibrated_run_groups, p3_gcm_glm_uncalibrated_output_feathers)
  ),
  
  # Group output feather tibble by spatial tile number
  tar_target(
    p3_gcm_glm_uncalibrated_output_feather_groups,
    p3_gcm_glm_uncalibrated_output_feather_tibble %>%
      group_by(spatial_tile_no) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Generate a zip file for each tile, zipping the grouped feathers
  tar_target(
    p3_gcm_glm_uncalibrated_output_zips,
    {
      files_to_zip <- p3_gcm_glm_uncalibrated_output_feather_groups$export_fl
      zipfile_out <- sprintf('3_extract/out/GLM_GCMs_tile%s.zip', unique(p3_gcm_glm_uncalibrated_output_feather_groups$spatial_tile_no))
      zip_output_files(files_to_zip, zipfile_out)
    },
    format = 'file',
    pattern = map(p3_gcm_glm_uncalibrated_output_feather_groups)
  ),
  
  ##### Extract NLDAS model output #####
  # Use grouped runs target to write NLDAS glm output to feather files
  # Function will generate the output feather file for each lake, 
  # truncating the output to the valid dates for the NLDAS time period 
  # (excluding the burn-in and burn-out periods) and saving only the 
  # temperature predictions for each depth and ice flags
  tar_target(
    p3_nldas_glm_uncalibrated_output_feathers,
    write_glm_output(p2_nldas_glm_uncalibrated_run_groups,
                     outfile_template='3_extract/out/GLM_%s_%s.feather'),
    pattern = map(p2_nldas_glm_uncalibrated_run_groups)
  ),
  
  # Zip all NLDAS output into a single zip file
  tar_target(
    p3_nldas_glm_uncalibrated_output_zips,
    zip_output_files(p3_nldas_glm_uncalibrated_output_feathers, '3_extract/out/GLM_NLDAS.zip'),
    format = 'file'
  )
)