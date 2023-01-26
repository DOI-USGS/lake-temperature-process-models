source('3_extract/src/process_glm_output.R')
source('3_extract/src/build_output_netCDFs.R')
source('3_extract/src/ncdfgeom_fxns.R')
source('4_visualize/src/plot_data_utility_fxns.R')

p3 <- list(
  
  # Setup table of GLM variable definitions to use in NetCDF file metadata
  tar_target(p3_nc_var_info,
             tibble(
               var_name = c("temp","ice"),
               longname = c("Surface water temperature [°C]","Boolean indicating presence of ice"),
               units = c("degrees Celcius","NA"),
               data_precision = c('float','integer'),
               compression_precision = c('.2','1')
             )),
  
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
  
  # Get vector of site_ids for which we have GCM output
  tar_target(p3_gcm_export_site_ids,
             p3_gcm_glm_uncalibrated_output_feather_tibble %>%
               arrange(site_id) %>%
               pull(site_id) %>%
               unique()),
  
  ###### Write GCM GLM output to netCDF - one per GCM per state ######
  # Group output feather tibble by gcm and by state
  tar_target(
    p3_gcm_glm_uncalibrated_output_feather_groups,
    p3_gcm_glm_uncalibrated_output_feather_tibble %>%
      filter(!(site_id == 'nhdhr_86165097')) %>% # filter out site w/ bad results
      group_by(driver, state) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Get vector of unique states for which we have GCM output
  tar_target(
    p3_gcm_glm_uncalibrated_output_states,
    p3_gcm_glm_uncalibrated_output_feather_groups %>%
      arrange(state) %>%
      pull(state) %>%
      unique()),
  
  # Get vector of unique GCM drivers for which we have output
  tar_target(
    p3_gcm_glm_uncalibrated_output_drivers,
    p3_gcm_glm_uncalibrated_output_feather_groups %>%
      arrange(driver) %>%
      pull(driver) %>%
      unique()),
  
  # Get vector of site_ids for which we have GCM output in each state - STATE SPECIFIC
  tar_target(p3_gcm_export_site_ids_states,
             p3_gcm_glm_uncalibrated_output_feather_groups %>%
               filter(state == p3_gcm_glm_uncalibrated_output_states) %>%
               arrange(site_id) %>%
               pull(site_id) %>%
               unique(),
             pattern = map(p3_gcm_glm_uncalibrated_output_states)),
  
  # Pull out lake depth for GCM export sites in each state - STATE SPECIFIC
  tar_target(
    p3_gcm_depths,
    purrr::map_df(p3_gcm_export_site_ids_states, function(site_id) {
      site_nml <- p1_gcm_nml_list_subset[[site_id]]
      tibble(
        site_id = site_id,
        lake_depth = site_nml$lake_depth
      )
    }),
    pattern = map(p3_gcm_export_site_ids_states)
  ),
  
  # Set vector of depths for which to keep temp preds, based on max depth of export lakes in each state - STATE SPECIFIC
  # currently matching Andy's approach here: https://github.com/USGS-R/lake-temperature-lstm-static/blob/main/2_process/process_config.yaml#L8-L13
  tar_target(
    p3_gcm_depths_export,
    {
      if (max(p3_gcm_depths$lake_depth, na.rm=TRUE) <= 10) {
        c(seq(0,10, by=0.5))
      } else if (max(p3_gcm_depths$lake_depth, na.rm=TRUE) > 10 & max(p3_gcm_depths$lake_depth, na.rm=TRUE) <= 20) {
        c(seq(0,10, by=0.5),seq(11,20,by=1))
      } else if (max(p3_gcm_depths$lake_depth, na.rm=TRUE) > 20 & max(p3_gcm_depths$lake_depth, na.rm=TRUE) <= 40) {
        c(seq(0,10, by=0.5),seq(11,19,by=1),seq(20,40, by=2))
      } else if (max(p3_gcm_depths$lake_depth, na.rm=TRUE) > 40  & max(p3_gcm_depths$lake_depth, na.rm=TRUE) <= 100) {
        c(seq(0,10, by=0.5),seq(11,19,by=1),seq(20,38, by=2),seq(40,100, by=5))
      } else if (max(p3_gcm_depths$lake_depth, na.rm=TRUE) > 100  & max(p3_gcm_depths$lake_depth, na.rm=TRUE) <= 200) {
        c(seq(0,10, by=0.5),seq(11,19,by=1),seq(20,38, by=2),seq(40,95, by=5), seq(100, 200, by=10))
      } else {
        c(seq(0,10, by=0.5),seq(11,19,by=1),seq(20,38, by=2),seq(40,95, by=5), seq(100, 190, by=10), seq(200, max(p3_gcm_depths$lake_depth, na.rm=TRUE),by=20))
      }
    },
    pattern = map(p3_gcm_depths)
  ),
  
  # Pull latitude and longitude coordinates for exported site_ids in each state - STATE SPECIFIC
  tar_target(p3_gcm_site_coords,
             pull_site_coords(p1_lake_centroids_sf_rds, p3_gcm_export_site_ids_states),
             pattern = map(p3_gcm_export_site_ids_states)),
  
  # Write GCM GLM output to netCDF files -- one per GCM, per state - GCM SPECIFIC *AND* STATE SPECIFIC 
  tar_target(
    p3_gcm_glm_uncalibrated_nc,
    {
      # check mapping to ensure is correct
      tar_assert_identical(p3_gcm_glm_uncalibrated_output_feather_groups$site_id, p3_gcm_site_coords$site_id, "p3_gcm_site_coords site ids don't match p3_gcm_glm_uncalibrated_output_feather_groups site ids")
      
      gcm_nc_dir <-'3_extract/out/lake_temp_preds_glm_gcm'
      if (!dir.exists(gcm_nc_dir)) dir.create(gcm_nc_dir)

      generate_output_nc(
        nc_file = file.path(gcm_nc_dir,
                            sprintf('lake_temp_preds_glm_gcm_%s_%s.nc', 
                                    unique(p3_gcm_glm_uncalibrated_output_feather_groups$driver),
                                    unique(p3_gcm_glm_uncalibrated_output_feather_groups$state))),
        output_info = p3_gcm_glm_uncalibrated_output_feather_groups,
        export_depths = p3_gcm_depths_export,
        nc_var_info = p3_nc_var_info,
        site_coords = p3_gcm_site_coords, 
        compression = TRUE)
    },
    format = 'file',
    pattern = map(p3_gcm_glm_uncalibrated_output_feather_groups,
                  cross(p3_gcm_glm_uncalibrated_output_drivers, map(p3_gcm_depths_export, p3_gcm_site_coords)))
  ),
  
  tar_target(
    p3_gcm_glm_uncalibrated_nc_tibble,
    tibble(
      driver = unique(p3_gcm_glm_uncalibrated_output_feather_groups$driver),
      state = unique(p3_gcm_glm_uncalibrated_output_feather_groups$state),
      nc_file = p3_gcm_glm_uncalibrated_nc,
      nc_file_hash = tools::md5sum(p3_gcm_glm_uncalibrated_nc)
    ),
    pattern = map(p3_gcm_glm_uncalibrated_output_feather_groups, p3_gcm_glm_uncalibrated_nc)
  ),
  
  tar_target(
    p3_gcm_glm_uncalibrated_nc_tibble_groups,
    p3_gcm_glm_uncalibrated_nc_tibble %>%
      group_by(driver) %>%
      tar_group(),
    iteration = 'group'
  ),
  
  # Zip GCM GLM netCDFs into single zipped folder
  tar_target(
    p3_gcm_glm_uncalibrated_nc_zip,
    {
      files_to_zip <- p3_gcm_glm_uncalibrated_nc
      zipfile_out <- sprintf('%s.zip', unique(dirname(p3_gcm_glm_uncalibrated_nc)))
      zip_output_files(files_to_zip, zipfile_out)
    },
    format = 'file'
  ),
  
  # Zip GCM GLM netCDFs into folders by GCM
  tar_target(
    p3_gcm_glm_uncalibrated_nc_state_zips,
    {
      files_to_zip <- p3_gcm_glm_uncalibrated_nc_tibble_groups$nc_file
      zipfile_out <- sprintf('%s_%s.zip', 
                             unique(dirname(p3_gcm_glm_uncalibrated_nc_tibble_groups$nc_file)), 
                             unique(p3_gcm_glm_uncalibrated_nc_tibble_groups$driver))
      zip_output_files(files_to_zip, zipfile_out)
    },
    pattern = map(p3_gcm_glm_uncalibrated_nc_tibble_groups),
    format = 'file'
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
  
  # Get vector of site_ids for which we have NLDAS output
  tar_target(p3_nldas_export_site_ids,
             p3_nldas_glm_uncalibrated_output_feather_tibble %>%
               arrange(site_id) %>%
               pull(site_id)),
  
  ###### Write NLDAS GLM output to netCDF - one per state ######
  # Group output feather tibble by state
  tar_target(
    p3_nldas_glm_uncalibrated_output_feather_groups,
    p3_nldas_glm_uncalibrated_output_feather_tibble %>%
      group_by(state) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Get vector of site_ids for which we have NLDAS output for each state
  tar_target(p3_nldas_export_site_ids_states,
             p3_nldas_glm_uncalibrated_output_feather_groups %>%
               arrange(site_id) %>%
               pull(site_id),
             pattern = map(p3_nldas_glm_uncalibrated_output_feather_groups)),

  # Pull out lake depth for NLDAS export sites within each state
  tar_target(
    p3_nldas_depths,
    purrr::map_df(p3_nldas_export_site_ids_states, function(site_id) {
      site_nml <- p1_nldas_nml_list_subset[[site_id]]
      tibble(
        site_id = site_id,
        lake_depth = site_nml$lake_depth
      )
    }),
    pattern = map(p3_nldas_export_site_ids_states)
  ),
  
  # Set vector of depths for which to keep temp preds, based on max depth of export lakes in each state
  # currently matching Andy's approach here: https://github.com/USGS-R/lake-temperature-lstm-static/blob/main/2_process/process_config.yaml#L8-L13
  tar_target(
    p3_nldas_depths_export,
    {
      if (max(p3_nldas_depths$lake_depth, na.rm=TRUE) <= 10) {
        c(seq(0,10, by=0.5))
      } else if (max(p3_nldas_depths$lake_depth, na.rm=TRUE) > 10 & max(p3_nldas_depths$lake_depth, na.rm=TRUE) <= 20) {
        c(seq(0,10, by=0.5),seq(11,20,by=1))
      } else if (max(p3_nldas_depths$lake_depth, na.rm=TRUE) > 20 & max(p3_nldas_depths$lake_depth, na.rm=TRUE) <= 40) {
        c(seq(0,10, by=0.5),seq(11,19,by=1),seq(20,40, by=2))
      } else if (max(p3_nldas_depths$lake_depth, na.rm=TRUE) > 40  & max(p3_nldas_depths$lake_depth, na.rm=TRUE) <= 100) {
        c(seq(0,10, by=0.5),seq(11,19,by=1),seq(20,38, by=2),seq(40,100, by=5))
      } else if (max(p3_nldas_depths$lake_depth, na.rm=TRUE) > 100  & max(p3_nldas_depths$lake_depth, na.rm=TRUE) <= 200) {
        c(seq(0,10, by=0.5),seq(11,19,by=1),seq(20,38, by=2),seq(40,95, by=5), seq(100, 200, by=10))
      } else {
        c(seq(0,10, by=0.5),seq(11,19,by=1),seq(20,38, by=2),seq(40,95, by=5), seq(100, 190, by=10), seq(200, max(p3_nldas_depths$lake_depth, na.rm=TRUE),by=20))
      }
    },
    pattern = map(p3_nldas_depths)
  ),
  
  # Pull latitude and longitude coordinates for exported site_ids in each state
  tar_target(p3_nldas_site_coords,
             pull_site_coords(p1_lake_centroids_sf_rds, p3_nldas_export_site_ids_states),
             pattern = map(p3_nldas_export_site_ids_states)), 

  # Write NLDAS GLM output for each state to a netCDF file
  tar_target(
    p3_nldas_glm_uncalibrated_nc,
    {
      nldas_nc_dir <- sprintf('3_extract/out/lake_temp_preds_glm_%s',
                              unique(p3_nldas_glm_uncalibrated_output_feather_groups$driver))
      if (!dir.exists(nldas_nc_dir)) dir.create(nldas_nc_dir)
                              
      generate_output_nc(
        nc_file = file.path(nldas_nc_dir,
                            sprintf('lake_temp_preds_glm_%s_%s.nc',
                                    unique(p3_nldas_glm_uncalibrated_output_feather_groups$driver),
                                    unique(p3_nldas_glm_uncalibrated_output_feather_groups$state))),
        output_info = p3_nldas_glm_uncalibrated_output_feather_groups,
        export_depths = p3_nldas_depths_export,
        nc_var_info = p3_nc_var_info,
        site_coords = p3_nldas_site_coords, 
        compression = TRUE)
    },
    pattern = map(p3_nldas_glm_uncalibrated_output_feather_groups, p3_nldas_depths_export, p3_nldas_site_coords),
    format = 'file'
  ),
  
  # Zip NLDAS GLM netCDFs into single zipped folder
  tar_target(
    p3_nldas_glm_uncalibrated_nc_zip,
    {
      files_to_zip <- p3_nldas_glm_uncalibrated_nc
      zipfile_out <- sprintf('%s.zip', unique(dirname(p3_nldas_glm_uncalibrated_nc)))
      zip_output_files(files_to_zip, zipfile_out)
    },
    format = 'file'
  ),
  
  ##### Generate combined run summary #####
  tar_render(p3_run_summary,
             '3_extract/out/GLM_run_summary.Rmd',
             packages = c('knitr'))
)