source('1_prep/src/munge_meteo.R')
source('1_prep/src/munge_nmls.R')

p1 <- list(
  # pull in files from lake-temperature-model-prep
  # TODO - transfer to Denali using globus
  # lake - GCM cell crosswalk
  tar_target(p1_lake_cell_xwalk_csv, '1_prep/in/lake_cell_xwalk.csv', format = 'file'),
  tar_target(p1_lake_cell_xwalk_df, readr::read_csv(p1_lake_cell_xwalk_csv, col_types=cols())),
  # list of lake-specific attributes for nml modification
  tar_target(p1_nml_list_rds, '1_prep/in/nml_list.rds', format = 'file'),

  # Define maping variables
  tar_target(p1_lake_ids, 
             p1_lake_cell_xwalk_df %>% 
               filter(site_id %in% names(readr::read_rds(p1_nml_list_rds))) %>% 
               pull(site_id)),
  tar_target(p1_cell_nos, unique(p1_lake_cell_xwalk_df$cell_no)),
  tar_target(p1_gcm_names, c('ACCESS', 'GFDL', 'CNRM', 'IPSL', 'MRI', 'MIROC')),
  tar_target(p1_gcm_dates, c('1980_1999', '2040_2059', '2080_2099')),
  
  # COMMENTING OUT FOR NOW, WHILE WE REFINE NETCDF APPROACH
  # # map over gcm names to read in netCDF files - one per GCM
  # tar_target(p1_gcm_ncs, {
  #   filename <- sprintf('1_prep/in/7_GCM_%s.nc', p1_gcm_names)
  #   return(filename)
  # }, format = 'file', pattern = map(p1_gcm_names)),
  # 
  
  # # TODO - come up with a more efficient way to split netCDF files
  # # right now, each netCDF file is read in many times
  # # split netCDF into feather files - by cell, GCM, and time period
  # tar_target(p1_meteo_feathers,
  #            munge_nc_files(p1_gcm_ncs, p1_gcm_names, p1_cell_nos, p1_gcm_dates, 
  #                           outfile_template = '1_prep/out/GCM_%s_%s_%s.feather'),
  #            format = 'file',
  #            pattern = cross(p1_cell_nos, map(p1_gcm_ncs, p1_gcm_names), p1_gcm_dates)),
  
  # FOR NOW, USE LINDSAY'S FEATHER FILES, BROUGHT IN MANUALLY
  # mapping over gcm_names, gcm_dates, and cell_nos to read in Lindsay's created feather files
  tar_target(p1_meteo_feathers, 
             {filename <- sprintf('1_prep/tmp/GCM_%s_%s_%s.feather', p1_gcm_names, p1_gcm_dates, p1_cell_nos)
             return(filename)},
             format = 'file',
             pattern = cross(p1_gcm_names, p1_gcm_dates, p1_cell_nos)),
  
  # build meteo xwalk
  tar_target(p1_meteo_xwalk,
             build_meteo_xwalk(p1_meteo_feathers, p1_lake_cell_xwalk_df, p1_gcm_names, p1_gcm_dates),
             iteration = 'group'),
  
  # Set up list of nml objects, with NULL for meteo_fl
  # Transform a single file of all lakes to a single list of all lakes
  # (subset to p1_lake_ids), then tell `targets` to think of that list as an iterable list
  tar_target(p1_glm_template_nml, '1_prep/in/glm3_template.nml', format = 'file'),
  tar_target(p1_nml_objects,
             munge_nmls(nml_list_rds = p1_nml_list_rds,
                        lake_ids = p1_lake_ids,
                        base_nml = p1_glm_template_nml),
             packages = c('glmtools'),
             iteration = 'list')
)
