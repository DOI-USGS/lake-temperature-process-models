#' @title Pull site coordinates
#' @description Get WGS84 latitude and longitude of lake centroids
#' for exported sites
#' @lake_centroids_sf_rds rds file of lake centroids sf object
#' @sites vector of site_ids
pull_site_coords <- function(lake_centroids_sf_rds, sites) {
  readRDS(lake_centroids_sf_rds) %>%
    filter(site_id %in% sites) %>%
    mutate(lon = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2]) %>%
    sf::st_set_geometry(NULL) %>%
    arrange(site_id)
}

#' @title Creates a NetCDF file storing the GLM output
#' @description Create a single NetCDF file containing temperature predictions at all depths in
#' each lake, across all time periods, for each GCM, as well as ice flag predictions for each GCM.
#' @param nc_file netcdf outfile name
#' @param output_info A tibble with one row per GLM output feather file which includes the 
#' site_id, driver, the name of the export feather file, its hash, and the state 
#' the lake is in
#' @param export_depths vector of depths for which we wish to export predictions
#' @param nc_var_info variables and descriptions to store in NetCDF
#' @param site_coords WGS84 coordinates of lake centroids
#' @param compression T/F if the nc file should be compressed after creation

# idea is to create the file with write_timeseries_dsg() by perhaps filling with the zero depth data, 
# then create a secondary function to add the depth dimension and add the additional data to those.
# that later modification would be done with RNetCDF per explorations here:
# https://github.com/hcorson-dosch/lake-temperature-model-prep/blob/bc4094cfb871a1b61dbfa78e4a9bae2c73789243/7_drivers_munge/src/GCM_driver_nc_utils.R#L101-L158
generate_output_nc <- function(nc_file, output_info, export_depths, nc_var_info, site_coords, compression) {
  # NOTE: adding a stop() for now while compression code and documentation still
  # needs to be refined further, but retaining draft code below
  if (compression == TRUE) {
    stop(paste('Compression is not fully supported at this time',
               'Please re-run with the compression parameter set to FALSE',
               sep='\n'))
  }
  
  # Delete nc outfile if it exists already
  if (file.exists(nc_file)) {
    unlink(nc_file)
  } 
  
  # If compressing netCDFs, create temporary nc file, and delete if exists already
  # The uncompressed netCDF will be saved to this temporary file rather than the outfile
  if (compression == TRUE) {
    compressed_outfile <- nc_file
    nc_file <- str_replace(nc_file, pattern = '.nc', '_uncompressed.nc')
    if (file.exists(nc_file)) unlink(nc_file)
  }
  
  # get vector of temp columns to keep, based on specified export depths
  export_depth_cols <- paste0('temp_', export_depths)
  
  # Read in all predictions for the current (tar_group) driver
  output_data <- purrr::pmap_df(output_info, function(...) {
    site_output_info <- tibble(...)
    site_output <- arrow::read_feather(site_output_info$export_fl)
    # determine which `temp_{depth}` columns to keep based on export depth columns
    cols_to_keep <- colnames(site_output)[colnames(site_output) %in% export_depth_cols]
    site_output_subset <- site_output %>%
      select(time, all_of(cols_to_keep), ice) %>%
      mutate(site_id = site_output_info$site_id, .before=1)
    return(site_output_subset)
  }) %>% arrange(site_id)
  
  ice_data <- output_data %>%
    select(site_id, time, ice)
  
  temp_data <- output_data %>%
    select(-ice)
  
  # Pull vector of unique dates and convert to POSIXct
  glm_dates <- unique(output_data$time)
  glm_dates_posixct <- as.POSIXct(glm_dates, tz='GMT')
  
  # Pull vector of unique site ids
  site_ids <- unique(output_info$site_id)
  site_ids_dim_name <- "site_id"
  
  # Get vectors of lake centroid latitudes and longitudes
  lake_centroid_lats <- site_coords %>% pull(lat)
  lake_centroid_lons <- site_coords %>% pull(lon)
  
  # Set up attributes for NetCDF
  data_time_units <- "days since 1970-01-01 00:00:00"
  source_info <- list('title' = sprintf('GLM predictions of lake temperature in MN lakes, driver: %s', unique(output_info$driver)))
  data_attributes <- source_info
  data_coordvar_long_names <- list(instance = "identifier for the lake location; NHDHR PermID", 
                                   time = "date of prediction",
                                   lat = "latitude of lake centroid",
                                   lon = "longitude of lake centroid")
  
  # Set up attributes for ice variable
  ice_metadata <- nc_var_info %>% filter(var_name=='ice')
  ice_data_unit <- rep(ice_metadata$units, length(site_ids))
  ice_data_prec <- ice_metadata$data_precision
  ice_data_metadata <- list(name = ice_metadata$var_name, long_name = ice_metadata$longname)

  ### CODE TO BUILD NETCDFS
  ## Write surface preds with write_timeseries_dsg()

  # Get ice flags in wide format
  # issue: need to filter and pivot - what is most efficient?
  # approach 1: convert dataframe to data.table. Set keys (driver, depth) and use keys to filter. transpose data.table
  # approach 2: loop through site ids, filter data to site, depth, driver. add data to matrix. convert matrix to data.frame
  # approach 3: use dplyr to pivot filtered tibble then convert to data.frame
  skipped_ids_ice <- c()
  ice_wide <- matrix(rep(NA_real_, length(glm_dates) * length(site_ids)), ncol = length(site_ids))
  for (i in 1:length(site_ids)) {
    this_site <- site_ids[i]
    this_site_ice <- ice_data %>% filter(site_id==this_site) %>% pull(ice)
    if (length(this_site_ice) == nrow(ice_wide)){
      ice_wide[, i] <- this_site_ice
    } else {
      skipped_ids_ice <- c(skipped_ids_ice, site_id)
    }
  }
  # convert to data.frame since that is what the write_timeseries_dsg() file expects
  ice_wide <- as.data.frame(ice_wide) %>% setNames(site_ids)
  
  # # pivoting approach
  # ice_wide <- ice_data %>%
  #   pivot_wider(id_cols = c("time"), names_from = site_id, values_from = ice) %>%
  #   select(-time) %>%
  #   as.data.frame()
  
  # Check if nc file already exists, and therefore should be added to,
  # or if it needs to be created
  var_add_to_existing <- ifelse(file.exists(nc_file), TRUE, FALSE)
  
  # write the ice flag predictions for a single GCM to the netcdf file:
  # ncdfgeom::write_timeseries_dsg() function documentation:
  # https://github.com/USGS-R/ncdfgeom/blob/master/R/write_timeseries_dsg.R
  ncdfgeom::write_timeseries_dsg(nc_file,
                                 instance_names = site_ids,
                                 lats = lake_centroid_lats,
                                 lons = lake_centroid_lons,
                                 times = glm_dates_posixct,
                                 data = ice_wide,
                                 data_unit = ice_data_unit,
                                 data_prec = ice_data_prec,
                                 data_metadata = ice_data_metadata,
                                 time_units = data_time_units,
                                 instance_dim_name = site_ids_dim_name,
                                 dsg_timeseries_id = site_ids_dim_name,
                                 attributes = data_attributes,
                                 coordvar_long_names = data_coordvar_long_names,
                                 add_to_existing = var_add_to_existing,
                                 overwrite = TRUE)

  ## modify and add new dims (depth, GCMs?) and temp preds with RNetCDF
  # For now, only add dim for depth (3D netCDF)
  nc <- RNetCDF::open.nc(nc_file, write=TRUE)
  
  # Modify global feature type and ice feature type
  att.put.nc(nc, "NC_GLOBAL", 'featureType', "NC_CHAR", 'timeSeriesProfile')
  att.put.nc(nc, ice_data_metadata$name, 'featureType', "NC_CHAR", 'timeSeries')
  
  # Add dimension for depth
  all_depths <- as.numeric(str_remove(output_data %>% select(starts_with('temp_')) %>% colnames(), 'temp_'))
  depths_dim_name <- "depth"
  n_depths <- length(all_depths)

  # Define depth dimension - type: float, units of meters
  dim.def.nc(nc, depths_dim_name, n_depths, unlim=FALSE)
  # set up GCM info using ncdfgeom function `add_var()`
  add_var(nc, depths_dim_name, c(depths_dim_name), "NC_DOUBLE", 'm', -999, long_name = 'Depth of prediction beneath lake surface', data=all_depths)
  # Populate the variable with the actual GCM names
  var.put.nc(nc, depths_dim_name, all_depths)
  # Set 'cf_role' attribute for depth - see http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/ch09s05.html
  att.put.nc(nc, depths_dim_name, 'cf_role', "NC_CHAR", "profile_id")

  # Set up temp var metadata
  temp_metadata <- nc_var_info %>% filter(var_name=='temp')
  temp_data_unit <- rep(temp_metadata$units, length(site_ids))
  temp_data_prec <- temp_metadata$data_precision
  temp_data_metadata <- list(name = temp_metadata$var_name, long_name = temp_metadata$longname)
  
  # set up temp variable
  add_var(nc, name = temp_data_metadata$name, dim = c("time", site_ids_dim_name, depths_dim_name),
          type = temp_data_prec, units = temp_data_unit, missing = -2147483648,
          long_name = temp_data_metadata[['long_name']])
  
  # Add coordinates
  coordinates <- paste('time','lat','lon') # paste(pkg.env$time_var_name,pkg.env$lat_coord_var_name,pkg.env$lon_coord_var_name)
  att.put.nc(nc, temp_data_metadata$name, 'coordinates', "NC_CHAR", coordinates)
  
  # add FeatureType information
  att.put.nc(nc, temp_data_metadata$name, 'featureType', "NC_CHAR", 'timeSeriesProfile')
  
  # Add temp data for all depths
  temp_dt <- as.data.table(temp_data)
  setkey(temp_dt, time)
  array_3d <- array(NA, dim = c(length(glm_dates), length(site_ids), n_depths))
  for (i in seq(length(glm_dates))) {
    temp_dt_date <- temp_dt[.(glm_dates[i])]
    temp_wide <- temp_dt_date[,c("site_id","time"):=NULL] # dim = n_rows = n_depths, n_cols = n_sites
    array_3d[i,,] <- as.matrix(temp_wide)
    remove(temp_dt_date)
    remove(temp_wide)
  }
  var.put.nc(nc, temp_data_metadata$name, array_3d, start=c(1,1,1), count=c(dim(array_3d)))
  
  # # Add dimension for GCMs
  # drivers <- unique(lake_gcm_info$driver)
  # drivers_dim_name <- "GCM"
  # n_drivers <- length(drivers)
  # # define dimensions
  # dim.def.nc(nc, drivers_dim_name, n_drivers, unlim=FALSE)
  # # set up GCM info using ncdfgeom function `add_var()`
  # add_var(nc, drivers_dim_name, c(drivers_dim_name), "NC_CHAR", long_name = 'Notaro debiased downscaled GCM', data=drivers)
  # # Populate the variable with the actual GCM names
  # var.put.nc(nc, drivers_dim_name, drivers)
  # 
  # # Loop through remaining GCMs to add surface temps
  # remaining_drivers <- drivers[!(drivers  %in% initial_driver)]
  # # put_data_in_nc(nc, nt, n, data_name, data, alts)
  # for (driver in remaining_drivers) {
  #   
  # }
  
  # loop through GCMs to add ice data
  # 3-dimensions: time, site_id, 
  # ice_metadata <- nc_var_info %>% filter(var_name=='ice')
  # data_name = ice_metadata[['name']]
  # for (driver in drivers) {
  #   
  # }
  
  # close netCDF
  RNetCDF::close.nc(nc)
  
  # If compressing netCDFs, compress the temporary netCDF file and save
  # to the final output file
  if (compression == TRUE) {
    # Note that the nc_file filename used above represents the temporary,
    # uncompressed nc file
    temp_nc_file <- nc_file
    # Redefine nc_file to be the original nc_file filename parameter,
    # stored above as `compressed_outfile`
    nc_file <- compressed_outfile
    
    # Run these ncdf commands from the directory of the files:
    project_dir <- setwd(dirname(nc_file))
    
    # Set up precision arguments for each variable using vars_info tibble
    # --ppc key1=val1#key2=val2
    precision_args <- paste(paste(vars_info$var_name, vars_info$compression_precision, sep = '='), collapse = '#')
    
    # Compress and quantize the file
    # This command requires that NCO be installed and able to be
    # called by R via system commands
    # see http://nco.sourceforge.net/
    system(sprintf("ncks -h --fl_fmt=netcdf4 --cnk_plc=g3d --cnk_dmn time,10 --ppc %s %s %s",
                   precision_args, basename(temp_nc_file), basename(nc_file)))
    # Switch back to the project directory
    setwd(project_dir)
    
    # Delete the temporary (uncompressed) file if the final compressed
    # file has been created. If it hasn't, throw an error.
    # Using this approach in place of tryCatch, since if NCO is the issue,
    # R will throw a system error message, *not* a console error
    if (file.exists(nc_file)) {
      unlink(temp_nc_file)
    } else {
      stop(paste(sprintf('The %s netCDF file could not be compressed',temp_nc_file),
                 'Make sure you have NCO netCDF operators installed on your system',
                 sep='\n'))
    }
  }
  return(nc_file)
}
