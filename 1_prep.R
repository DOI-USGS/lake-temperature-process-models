source('1_prep/src/munge_netCDFs.R')
source('1_prep/src/build_model_config.R')
source('1_prep/src/munge_nmls.R')

p1 <- list(
  # Define mapping variables
  tar_target(p1_gcm_names, c('ACCESS', 'GFDL', 'CNRM', 'IPSL', 'MRI', 'MIROC5')),
  tar_target(p1_gcm_time_periods, c('1981_2000', '2040_2059', '2080_2099')),
  
  ### Pull in files from lake-temperature-model-prep
  # TODO - transfer to Denali using globus
  # list of lake-specific attributes for nml modification
  # file copied from lake-temperature-model-prep repo
  tar_target(p1_nml_list_rds, '1_prep/in/nml_list.rds', format = 'file'),
  tar_target(p1_nml_site_ids, names(readr::read_rds(p1_nml_list_rds))),
  # lake - GCM cell - GCM tile crosswalk, filtered to only those lakes
  # that are in nml list (able to be modeled by GLM)
  # file copied from lake-temperature-model-prep repo
  tar_target(p1_lake_cell_tile_xwalk_csv, '1_prep/in/lake_cell_tile_xwalk.csv', format = 'file'),
  tar_target(p1_lake_cell_tile_xwalk_df, 
             readr::read_csv(p1_lake_cell_tile_xwalk_csv, col_types=cols()) %>%
               filter(site_id %in% p1_nml_site_ids) %>%
               arrange(site_id)),
  # NetCDF files with munged GCM driver data (one per GCM)
  # files copied from lake-temperature-model-prep repo
  tar_target(p1_gcm_ncs, 
             {
               filename <- sprintf('1_prep/in/GCM_%s.nc', p1_gcm_names)
               return(filename)
               }, 
             format = 'file',
             pattern = map(p1_gcm_names)),

  # Pull vectors of site ids and data cell numbers
  tar_target(p1_site_ids, p1_lake_cell_tile_xwalk_df %>% pull(site_id)),
  tar_target(p1_cell_nos, unique(p1_lake_cell_tile_xwalk_df %>% pull(data_cell_no))),

  # Specify length of desired burn-in and burn-out periods, in days
  tar_target(p1_burn_in, 300),
  tar_target(p1_burn_out, 190),
  # Pull out start and end dates of raw meteo data
  # and note start of burn-in and end of burn-out
  # TODO: won't catch when requested burn in is longer than
  # meteo and therefore isn't added in add_burn_in_out_to_meteo()
  tar_target(p1_gcm_dates,
             munge_gcm_dates(p1_gcm_time_periods, 
                            p1_gcm_ncs,
                            burn_in = p1_burn_in,
                            burn_out = p1_burn_out)),
  # Generate GCM/time-period/cell-specific csv files
  tar_target(p1_meteo_csvs,
             munge_nc_files(p1_gcm_ncs, 
                            p1_gcm_names, 
                            p1_cell_nos,
                            p1_gcm_time_periods,
                            burn_in = p1_burn_in,
                            burn_out = p1_burn_out,
                            outfile_template = '1_prep/out/GCM_%s_%s_%s.csv'),
             format = 'file',
             pattern = map(p1_gcm_ncs, p1_gcm_names)),

  # build model config
  tar_target(p1_model_config,
             build_model_config(p1_meteo_csvs, p1_lake_cell_tile_xwalk_df, p1_gcm_names, p1_gcm_dates)),
  
  # Set up list of nml objects, with NULL for meteo_fl, and custom depth parameters
  # Transform a single file of all lakes to a single list of all lakes
  # (subset to p1_site_ids), then tell `targets` to think of that list as an iterable list
  tar_target(p1_glm_template_nml, '1_prep/in/glm3_template.nml', format = 'file'),
  tar_target(p1_nml_objects,
             munge_nmls(nml_list_rds = p1_nml_list_rds,
                        site_ids = p1_site_ids,
                        base_nml = p1_glm_template_nml),
             packages = c('glmtools'),
             iteration = 'list')
)

