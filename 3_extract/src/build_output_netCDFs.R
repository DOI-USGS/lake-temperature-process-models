# Create NetCDF of all uncalibrated GLM output

# ncdfgeom function https://github.com/USGS-R/ncdfgeom/blob/main/R/0_ncdfgeom.R
add_var <- function(nc, name, dim, type, units = NA, missing = NA, long_name = NA, char_dim_len = NULL, data = NULL) {
  
  if(type == "NC_CHAR") {
    suppressWarnings(if(is.null(char_dim_len) & is.null(data)) stop("can't determine character dim length"))
    if(is.null(char_dim_len)) suppressWarnings(char_dim_len <- max(sapply(data, function(x) max(nchar(x), 
                                                                                                na.rm = TRUE)), 
                                                                   na.rm = TRUE))
    char_dim <- paste0(name,"_char")
    dim.def.nc(nc, char_dim, char_dim_len, unlim = FALSE)
    dim <- c(char_dim, dim)
  }
  var.def.nc(nc, name, type, dim)
  if(!any(is.na(units)))
    att.put.nc(nc, name, "units", "NC_CHAR", units)
  if(!is.na(missing))
    att.put.nc(nc, name, "missing_value", type, missing)
  if(!is.na(long_name))
    att.put.nc(nc, name, "long_name", "NC_CHAR", long_name)
}


#' @title Creates a NetCDF file storing the GLM output
#' @description Create a single NetCDF file containing temperature predictions at all depths in
#' each lake, across all time periods, for each GCM, as well as ice flag predictions for each GCM.
#' @param nc_file netcdf outfile name
#' @param output_info XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param nc_var_info variables and descriptions to store in NetCDF
#' @param site_coords WGS84 coordinates of lake centroids
#' @param compression T/F if the nc file should be compressed after creation

# idea is to create the file with write_timeseries_dsg() by perhaps filling with the zero depth data, 
# then create a secondary function to add the depth dimension and add the additional data to those.
# that later modification would be done with RNetCDF per explorations here:
# https://github.com/hcorson-dosch/lake-temperature-model-prep/blob/bc4094cfb871a1b61dbfa78e4a9bae2c73789243/7_drivers_munge/src/GCM_driver_nc_utils.R#L101-L158
generate_output_nc <- function(nc_file, output_info, nc_var_info, site_coords, compression) {
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
  
  # Read in all predictions for the current (tar_group) driver
  output_data <- purrr::pmap_df(output_info, function(...) {
    site_output_info <- tibble(...)
    arrow::read_feather(site_output_info$export_fl) %>%
      mutate(site_id = site_output_info$site_id, .before=1)
  }) %>% arrange(site_id)
  
  output_data_long <- munge_long(output_data)
  
  ice_data <- output_data_long %>%
    filter(depth == 0) %>%
    select(site_id, time, ice)
  
  temp_data <- output_data_long %>%
    select(-ice)
  
  # Pull vector of unique dates and convert to POSIXct
  glm_dates <- as.POSIXct(unique(output_data$time), tz='GMT')
  
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
  skipped_ids <- c()
  ice_wide <- matrix(rep(NA_real_, length(glm_dates) * length(site_ids)), ncol = length(site_ids))
  for (i in 1:length(site_ids)) {
    this_site <- site_ids[i]
    this_site_ice <- ice_data %>% filter(site_id==this_site) %>% pull(ice) #this_site_surface <- temp_data %>% filter(site_id==this_site, depth==0, driver==initial_driver) %>% pull(temperature)
    if (length(this_site_ice) == nrow(ice_wide)){
      ice_wide[, i] <- this_site_ice
    } else {
      skipped_ids <- c(skipped_ids, site_id)
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
                                 times = glm_dates,
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
  
  
  # Add dimension for depth
  all_depths <- unique(temp_data$depth)
  depths_dim_name <- "depth"
  n_depths <- length(all_depths)

  # Define depth dimension - type: float, units of meters
  dim.def.nc(nc, depths_dim_name, n_depths, unlim=FALSE)
  # set up GCM info using ncdfgeom function `add_var()`
  add_var(nc, depths_dim_name, c(depths_dim_name), "NC_DOUBLE", 'm', -999, long_name = 'Depth of prediction beneath lake surface', data=all_depths)
  # Populate the variable with the actual GCM names
  var.put.nc(nc, depths_dim_name, all_depths)

  # Set up temp var metadata
  temp_metadata <- nc_var_info %>% filter(var_name=='temp')
  temp_data_unit <- rep(temp_metadata$units, length(site_ids))
  temp_data_prec <- temp_metadata$data_precision
  temp_data_metadata <- list(name = temp_metadata$var_name, long_name = temp_metadata$longname)
  
  # set up temp variable
  add_var(nc, name = temp_data_metadata$name, dim = c(depths_dim_name, "time", site_ids_dim_name),
          type = temp_data_prec, units = temp_data_unit, missing = -2147483648,
          long_name = temp_data_metadata[['long_name']])
  
  # Add temp data for all depths
  
  
  
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




#### Example code from packaging GCM data into netCDFs
# Munging driver data from geoknife to create NetCDF for each GCM.

#' @title Creates a NetCDF file with GCM driver data
#' @description For each of the GCMs, create a single NetCDF file containing all the
#' variables and all the grid cells.
#' @param nc_file netcdf outfile name
#' @param gcm_raw_files vector of feather file paths, one for each tile with data, for the current GCM.
#' @param vars_info variables and descriptions to store in NetCDF
#' @param grid_params grid cell parameters to add to NetCDF as global attributes
#' @param spatial_info cell_nos, x indices, y indices, and WGS84 coordinates of grid cell centroids
#' @param gcm_name name of the current GCM
#' @param compression T/F if the nc file should be compressed after creation
generate_gcm_nc <- function(nc_file, gcm_raw_files, vars_info, grid_params, spatial_info,
                            gcm_name, compression) {
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
  
  # Read in all data for the current (tar_group) GCM
  gcm_data <- purrr::map_df(gcm_raw_files, function(gcm_raw_file) {
    arrow::read_feather(gcm_raw_file)
  }) %>% arrange(cell_no)
  
  # Pull vector of unique dates and convert to POSIXct
  gcm_dates <- as.POSIXct(unique(gcm_data$time), tz='GMT')
  
  # Pull vector of unique cell numbers
  gcm_cells <- unique(gcm_data$cell_no)
  gcm_cells_dim_name <- "gcm_cell_id"
  
  # Get WGS84 latitude and longitude of cell centroids
  cell_coords <- spatial_info %>%
    mutate(lon = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2]) %>%
    sf::st_set_geometry(NULL) %>%
    arrange(cell_no) %>%
    filter(cell_no %in% gcm_cells)
  
  # Get vectors of cell centroid latitudes and longitudes
  cell_centroid_lats <- cell_coords %>% pull(lat)
  cell_centroid_lons <- cell_coords %>% pull(lon)
  
  # Set up attributes for NetCDF that are independent of variables
  data_time_units <- "days since 1970-01-01 00:00:00"
  source_info <- list('title' = sprintf("Notaro debiased downscaled GCM data - %s", gcm_name),
                      'source' = paste("These data were derived from a source raster dataset of dynamically downscaled and debiased",
                                       "General Circulation Model (GCM) data generated by Michael Notaro (mnotaro@wisc.edu)",
                                       "and others (see https://ccr.nelson.wisc.edu/dynamical-downscaling/index.php).",
                                       "Global attributes of this netCDF that describe the source raster dataset are",
                                       "prefixed by 'source_raster_'. The projected coordinates for the source raster",
                                       "grid cell centroids can be computed from the WGS84 latitude and longtitude",
                                       "coordinates provided in this dataset using the source_raster_crs attribute", sep=' '))
  # add prefix to names of grid parameters to indicate that they refer to the source raster
  names(grid_params) <- paste0('source_raster_', names(grid_params))
  data_attributes <- append(source_info, grid_params)
  data_coordvar_long_names <- list(instance = "identifier for reconstructed Notaro downscaled GCM grid cell", time = "date",
                                   lat = "WGS84 latitude of downscaled grid cell centroid",
                                   lon = "WGS84 longitude of downscaled grid cell centroid")
  
  # Pivot data to long format to set up for filtering by variable
  gcm_data_long <- gcm_data %>%
    pivot_longer(cols = -c(time, cell_no), names_to = "variable", values_to = "value") %>%
    arrange(cell_no)
  
  # Loop over data variables to populate NetCDF using ncdfgeom::write_timeseries_dsg()
  for (variable in vars_info$var_name) {
    # Filter long-format data to selected variable
    variable_metadata <- vars_info %>% filter(var_name == variable)
    var_data <- gcm_data_long %>%
      filter(variable == !!variable) %>%
      pivot_wider(id_cols = c("time"), names_from = cell_no, values_from = value) %>%
      select(-time) %>%
      as.data.frame() %>%
      setNames(gcm_cells)
    
    # Pull attributes for selected variable
    var_data_unit <- rep(variable_metadata$units, length(gcm_cells))
    var_data_prec <- variable_metadata$data_precision
    var_data_metadata <- list(name = variable_metadata$var_name, long_name = variable_metadata$longname)
    
    # Check if nc file already exists, and therefore should be added to,
    # or if it needs to be created
    var_add_to_existing <- ifelse(file.exists(nc_file), TRUE, FALSE)
    
    # write this variable to the netcdf file:
    # ncdfgeom::write_timeseries_dsg() function documentation:
    # https://github.com/USGS-R/ncdfgeom/blob/master/R/write_timeseries_dsg.R
    ncdfgeom::write_timeseries_dsg(nc_file,
                                   instance_names = gcm_cells,
                                   lats = cell_centroid_lats,
                                   lons = cell_centroid_lons,
                                   times = gcm_dates,
                                   data = var_data,
                                   data_unit = var_data_unit,
                                   data_prec = var_data_prec,
                                   data_metadata = var_data_metadata,
                                   time_units = data_time_units,
                                   instance_dim_name = gcm_cells_dim_name,
                                   dsg_timeseries_id = gcm_cells_dim_name,
                                   attributes = data_attributes,
                                   coordvar_long_names = data_coordvar_long_names,
                                   add_to_existing = var_add_to_existing,
                                   overwrite = TRUE)
  }
  
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