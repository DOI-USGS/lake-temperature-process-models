source('3_extract/src/process_glm_output.R')
source('3_extract/src/build_output_netCDFs.R')
source('4_visualize/src/plot_data_utility_fxns.R')

p3 <- list(
  ##### Extract GCM model output #####
  # Use grouped runs target to combine GCM glm output into feather files
  # Function will generate the output feather file for each lake-gcm combo
  # across all three time periods, truncating the output to the valid dates 
  # for each time period (excluding the burn-in and burn-out periods) and
  # saving only the temperature predictions for each depth and ice flags
  tar_target(
    p3_gcm_glm_uncalibrated_output_feathers,
    {
      output_files <- p2_gcm_glm_uncalibrated_run_groups %>% # Already grouped by site_id
        group_by(driver) %>% # Also group by driver (GCM) for creating export files
        group_map(~ write_glm_output(.x, outfile_template='3_extract/out/GLM_%s_%s.feather'),
                  .keep = TRUE) %>%
        unlist()
      return(output_files)
    },
    format = 'file',
    pattern = map(p2_gcm_glm_uncalibrated_run_groups)),
  
  # Generate a tibble with a row for each output file
  # that includes the filename and its hash along with the
  # site_id, driver (gcm name), the state the lake is in,  
  # and the cell_no and tile_no for that lake.
  tar_target(
    p3_gcm_glm_uncalibrated_output_feather_tibble,
    generate_output_tibble(p3_gcm_glm_uncalibrated_output_feathers,
                           output_file_regex = "GLM_(.*)_(.*).feather",
                           lake_xwalk = p1_lake_cell_tile_xwalk_df)
  ),
  
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
  
  # Group output feather tibble by gcm
  tar_target(
    p3_gcm_glm_uncalibrated_output_feather_groups,
    p3_gcm_glm_uncalibrated_output_feather_tibble %>%
      group_by(driver) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Here, could put data into netCDF - if putting it by GCM, would want to group by GCM
  # Makes sense to work off ^ or similar tibble so can track hash of output data
  # Could work with raw feather output or ^ output feathers, which are just temp and ice
  # Setup table of GLM variable definitions to use in NetCDF file metadata
  tar_target(p3_gcm_export_site_ids,
             p3_gcm_glm_uncalibrated_output_feather_tibble %>%
               pull(site_id) %>%
               unique()),
  tar_target(p3_gcm_site_coords,
             # Get WGS84 latitude and longitude of lake centroids
             p1_gcm_lake_centroids_sf %>%
               filter(site_id %in% p3_gcm_export_site_ids) %>%
               mutate(lon = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2]) %>%
               sf::st_set_geometry(NULL) %>%
               arrange(site_id)),
  tar_target(p3_nc_var_info,
             tibble(
               var_name = c("temp","ice"),
               longname = c("Surface water temperature [°C]","Boolean indicating presence of ice"),
               units = c("degrees Celcius","NA"),
               data_precision = c('float','integer'),
               compression_precision = c('.2','1')
             )),
  # tar_target(
  #   p3_gcm_glm_uncalibrated_output_long,
  #   p3_gcm_glm_uncalibrated_output %>%
  #     pivot_longer(starts_with("temp_"), names_to="depth", values_to="temperature") %>%
  #     mutate(depth = as.numeric(str_remove(depth, 'temp_'))) %>%
  #     mutate(site_id = p3_gcm_glm_uncalibrated_output_feather_tibble$site_id,
  #            driver = p3_gcm_glm_uncalibrated_output_feather_tibble$driver, .before=1),
  #   pattern = map(p3_gcm_glm_uncalibrated_output, p3_gcm_glm_uncalibrated_output_feather_tibble)
  # ),
  tar_target(
    p3_gcm_glm_uncalibrated_nc,
    generate_output_nc(
      nc_file = sprintf('3_extract/out/GLM_GCMs_%s.nc', unique(p3_gcm_glm_uncalibrated_output_feather_groups$driver)), 
      output_info = p3_gcm_glm_uncalibrated_output_feather_groups,
      nc_var_info = p3_nc_var_info,
      site_coords = p3_gcm_site_coords, 
      compression = FALSE),
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
    format = 'file',
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
  )
)