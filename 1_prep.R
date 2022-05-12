source('1_prep/src/munge_meteo.R')
source('1_prep/src/build_model_config.R')
source('1_prep/src/munge_nmls.R')

p1 <- list(
  # Pull in GLM 3 template
  tar_target(p1_glm_template_nml, '1_prep/in/glm3_template.nml', format = 'file'),
  
  ##### Pull in files from lake-temperature-model-prep #####
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
               arrange(site_id) %>% #),
               filter(site_id %in% c('nhdhr_105567868', 'nhdhr_105569506', 'nhdhr_105569520', 'nhdhr_114336097', 'nhdhr_120019185','nhdhr_114544667'))), #   
  # Pull vector of site ids
  tar_target(p1_site_ids, p1_lake_cell_tile_xwalk_df %>% pull(site_id)),
  # Subset nml list
  tar_target(p1_nml_list_subset, readr::read_rds(p1_nml_list_rds)[p1_site_ids]),
  
  ##### GCM model set up #####
  # Define mapping variables
  tar_target(p1_gcm_names, c('ACCESS', 'GFDL', 'CNRM', 'IPSL', 'MRI', 'MIROC5')),
  tar_target(p1_gcm_time_periods, c('1981_2000', '2040_2059', '2080_2099')),
  # NetCDF files with munged GCM driver data (one per GCM)
  # files copied from lake-temperature-model-prep repo
  tar_target(p1_gcm_ncs, 
             {
               filename <- sprintf('1_prep/in/GCM_%s.nc', p1_gcm_names)
               return(filename)
               }, 
             format = 'file',
             pattern = map(p1_gcm_names)),

  # Pull vectors data cell numbers
  tar_target(p1_gcm_cell_nos, unique(p1_lake_cell_tile_xwalk_df %>% pull(data_cell_no))),

  # Specify length of desired burn-in and burn-out periods, in days
  tar_target(p1_gcm_burn_in, 300),
  tar_target(p1_gcm_burn_out, 190),
  # Pull out start and end dates of raw meteo data
  # and note start of burn-in and end of burn-out
  tar_target(p1_gcm_dates,
             munge_gcm_dates(p1_gcm_ncs,
                             p1_gcm_time_periods,
                             burn_in = p1_gcm_burn_in,
                             burn_out = p1_gcm_burn_out)),
  # Generate GCM/time-period/cell-specific csv files
  tar_target(p1_gcm_csvs,
             munge_gcm_nc_files(p1_gcm_ncs, 
                            p1_gcm_names, 
                            p1_gcm_cell_nos,
                            p1_gcm_dates,
                            outfile_template = '1_prep/out/GCM_%s_%s_%s.csv'),
             format = 'file',
             pattern = map(p1_gcm_ncs, p1_gcm_names)),

  # build model config
  tar_target(p1_gcm_model_config,
             build_gcm_model_config(p1_gcm_csvs, p1_lake_cell_tile_xwalk_df, p1_gcm_names, p1_gcm_dates)),
  
  # Set up list of nml objects, with NULL for meteo_fl, and custom depth parameters
  # Transform a single file of all lakes to a single list of all lakes
  # (subset to p1_site_ids), then tell `targets` to think of that list as an iterable list
  
  tar_target(p1_gcm_nml_objects,
             munge_model_nmls(nml_list = p1_nml_list_subset,
                        base_nml = p1_glm_template_nml,
                        driver_type = 'gcm'),
             packages = c('glmtools'),
             iteration = 'list'),
  
  ##### NLDAS model set up #####
  tar_target(p1_nldas_time_period, c('1980_2021')),
  # track unique NLDAS files
  # length(p1_nldas_csvs) = # of unique NLDAS files associated with p1_site_ids
  tar_target(p1_nldas_csvs,
             {
               nldas_files <- c()
               for (site_id in p1_site_ids) {
                 site_nml <- p1_nml_list_subset[[site_id]]
                 site_nldas_file <- file.path('1_prep/in/NLDAS_GLM_csvs', site_nml$meteo_fl)
                 if (!(site_nldas_file %in% nldas_files)) {
                   nldas_files <- c(nldas_files, site_nldas_file)
                 }
               }
               return(nldas_files)
             },
             format = 'file'
  ),
  
  # Define model start and end dates and note start of
  # burn-in and end of burn-out based on extent of NLDAS data
  # (using 1/2/1979 - 12/31/1979 for burn-in, and 1/1/2021 - 
  # 4/11/2021 for burn-out)
  tar_target(
    p1_nldas_dates,
    munge_nldas_dates(p1_nldas_csvs, p1_nldas_time_period)),

  
  # Set up nmls for NLDAS model runs
  tar_target(p1_nldas_nml_objects,
             munge_model_nmls(nml_list = p1_nml_list_subset,
                              base_nml = p1_glm_template_nml,
                              driver_type = 'nldas'),
             packages = c('glmtools'),
             iteration = 'list'),
  
  
  # Set up NLDAS model config
  tar_target(p1_nldas_model_config,
             build_nldas_model_config(p1_site_ids, p1_nldas_nml_objects, p1_nldas_csvs, p1_nldas_dates)
  )
)

