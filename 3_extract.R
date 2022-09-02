source('3_extract/src/process_glm_output.R')

p3 <- list(
  ##### Extract GCM model output #####
  # Use grouped runs target to combine GCM glm output into feather files
  # write_glm_output() will generate the output feather file for each lake-gcm combo
  # across all three time periods, truncating the output to the valid dates
  # for each time period (excluding the burn-in and burn-out periods) and
  # saving only the temperature predictions for each depth and ice flags
  # THEN, with generate_output_tibble(), generate a tibble with a row for each 
  # output file that includes the filename and its hash along with the site_id, 
  # driver (gcm name), the state the lake is in, and the cell_no and tile_no for 
  # that lake.
  tar_target(
    p3_gcm_glm_uncalibrated_output_feather_tibble,
    {
      feather_tibble <- p2_gcm_glm_uncalibrated_run_groups %>% # Already grouped by site_id
        group_by(driver) %>% # Also group by driver (GCM) for creating export files
        group_modify(~ {
          output_file <- write_glm_output(.x, outfile_template='3_extract/out/GLM_%s_%s.feather')
          output_tibble <- generate_output_tibble(output_file,
                                 output_file_regex = "GLM_(.*)_(.*).feather",
                                 lake_xwalk = p1_lake_cell_tile_xwalk_df) %>%
            select(-driver)
        }, .keep=TRUE) %>%
        select(site_id, driver, export_fl, export_fl_hash, everything())
      return(feather_tibble)
    },
    pattern = map(p2_gcm_glm_uncalibrated_run_groups)),
  
  # Save summary of output files
  tar_target(
    p3_gcm_glm_uncalibrated_output_summary_csv,
    {
      outfile <- '3_extract/out/GLM_GCM_summary.csv'
      p3_gcm_glm_uncalibrated_output_feather_tibble %>%
        select(site_id, driver, export_fl, export_fl_hash, state) %>%
        readr::write_csv(outfile)
      return(outfile)
    },
    format = 'file'
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
  
  # Generate a tibble with a row for each output file
  # that includes the filename and its hash along with the
  # site_id, driver (NLDAS), and the state the lake is in
  tar_target(
    p3_nldas_glm_uncalibrated_output_feather_tibble,
    generate_output_tibble(p3_nldas_glm_uncalibrated_output_feathers, 
                           output_file_regex = "GLM_(.*)_(.*).feather",
                           lake_xwalk = p1_lake_to_state_xwalk_df)
  ),
  
  # Save summary of output files
  tar_target(
    p3_nldas_glm_uncalibrated_output_summary_csv,
    {
      outfile <- '3_extract/out/GLM_NLDAS_summary.csv'
      readr::write_csv(p3_nldas_glm_uncalibrated_output_feather_tibble, outfile)
      return(outfile)
    },
    format = 'file'
  ),
  
  # Group output feather tibble by state
  tar_target(
    p3_nldas_glm_uncalibrated_output_feather_groups,
    p3_nldas_glm_uncalibrated_output_feather_tibble %>%
      group_by(state) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Generate a zip file for each state, zipping the grouped feathers
  tar_target(
    p3_nldas_glm_uncalibrated_output_zips,
    {
      files_to_zip <- p3_nldas_glm_uncalibrated_output_feather_groups$export_fl
      zipfile_out <- sprintf('3_extract/out/GLM_NLDAS_%s.zip', unique(p3_nldas_glm_uncalibrated_output_feather_groups$state))
      zip_output_files(files_to_zip, zipfile_out)
    },
    format = 'file',
    pattern = map(p3_nldas_glm_uncalibrated_output_feather_groups)
  ),
  
  ##### Generate combined run summary #####
  tar_render(p3_run_summary,
             '3_extract/out/GLM_run_summary.Rmd',
             packages = c('knitr'))
)