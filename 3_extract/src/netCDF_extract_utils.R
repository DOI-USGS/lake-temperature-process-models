#' @title read timeseries profile discrete sampling geometry netCDF
#' @description read in information about the coordinates and variables in
#' a discrete sampling geometry formatted netCDF file, and optionally
#' read in the data associated with each variable as well.
#' @param nc_file the filepath for the netCDF
#' @param read_data boolean - should the actual data be read in, or 
#' only the information about the netCDF coordinates, dimensions, 
#' and variables. Default = FALSE
#' @returns a ncdfgeom with 10 elements: time (dates), lats (latitudes 
#' of lake centroids), lons (longitudes of lake centroids), alts (empty),
#' depth (depths associated with temperature profiles), timeseries_id 
#' (the lake site ids), varmeta (metadata for the variables), data_unit
#' (units for the variables), data_precision (precision of the variables),
#' and global attributes (netCDF title)
read_timeseries_profile_dsg  <- function(nc_file, read_data=FALSE) {
  nc <-  open.nc(nc_file)
  on.exit(close.nc(nc), add  = TRUE)
  
  nc_meta <- get_nc_meta(nc)
  
  dsg <- get_dsg_meta(nc, nc_meta)
  # add depth variable
  dsg$depth <- filter(dsg$sn, .data$value == "depth")
  # Add profile id variable since featureType = timeSeriesProfile
  dsg$profile_id <- get_profile_id(nc_meta$attribute)
  
  nc_list <- get_nc_list(nc, dsg, nc_meta, read_data)
  
  return(add_globals(nc_list, nc_meta))
}

#' @title pull data for sites
#' @description for the specified `sites`, pull data for the specified
#' variable (`var`). The data will be pulled for all dates (for `var`
#' = 'temp' | 'ice') and for all depths (if `var` is 'temp')
#' @param nc the open netCDF object
#' @param nc_info the information about the open netCDF, pulled using
#' `read_timeseries_profile_dsg()`
#' @param var the variable for which data should be pulled. Must be
#' either 'ice' or 'temp'
#' @param sites a vector of site ids for which data will be pulled
#' @param long_format boolean - should the returned data be in long format
#' or not. Default = FALSE
#' @returns a dataframe in long or wide format containing the data for
#' the specified variable for the specified sites
pull_data_for_sites <- function(nc, nc_info, var, sites, long_format = FALSE) {
  
  # Parameter checks
  if (!(var %in% c('ice', 'temp'))) {
    stop(sprintf("The provided var argument (%s) is not a valid variable choice. Valid options are 'ice' or 'temp'", var))
  } 

  sites_not_in_data <- sites[!(sites %in% nc_info$timeseries_id)]
  if (length(sites_not_in_data) > 0) {
    stop(sprintf("Data for site(s) %s are not included in the specified netCDF file", paste(sites_not_in_data, collapse=', ')))
  }
  
  # If possible, group sites based on indices
  sites <- sites[order(match(sites, nc_info$timeseries_id))] # sort sites based on order in netCDF
  site_indices <- purrr::map(sites, ~ which(nc_info$timeseries_id == .x)) %>% unlist()
  index_groups <- cumsum(c(1L, diff(site_indices) != 1))
  site_index_groups <- split(site_indices, index_groups)
  
  # Pull data for requested variable for requested sites
  data_subset <- purrr::map_dfc(site_index_groups, function(index_group) {
    # Find sites associated w/ set of indices
    sites <- nc_info$timeseries_id[index_group]
    var_data <- as.data.frame(var.get.nc(nc, var, 
                                         start =c(1, index_group[1], 1), 
                                         count = c(length(nc_info$time), length(index_group), length(nc_info$depth)))) 
    
    if (var == 'temp') {
      colnames(var_data) <- paste0(rep(sites, length(nc_info$depth)),'_', rep(nc_info$depth, each=length(sites)))
    } else {
      colnames(var_data) <- as.character(sites)
    }
    return(var_data)
  }) %>%
    mutate(time = nc_info$time, .before = 1)
  
  # If var is temp, remove excess depth columns (> lake depth), where all temp values = NA
  if (var == 'temp') {
    data_subset <- data_subset %>%
      select(
        where(
          ~sum(!is.na(.x)) > 0
        )
      )
  }
  
  # If requested, transform into long format
  if (long_format && var == 'temp') {
    data_subset <- data_subset %>%
      pivot_longer(cols = (-time), names_to = c('site_id','depth'), names_pattern = '(.*)_(.*)', values_to='temperature') %>%
      mutate(depth = as.numeric(depth)) %>%
      arrange(site_id, time)
  }  else if (long_format && var == 'ice') {
    data_subset <- data_subset %>% 
      pivot_longer(cols = (-time), names_to = 'site_id', values_to = 'ice') %>%
      arrange(site_id, time)
  }
  
  return(data_subset)
}

###### Functions needed to run read_timeseries_profile_dsg() ###### 
# https://github.com/USGS-R/ncdfgeom/blob/main/R/read_timeseries_dsg.R#L57-L62
# NOT MODIFIED
get_nc_meta <- function(nc) {
  nc_meta <- nc_meta(nc)
  nc_meta$attribute <- check_timeseries_atts(nc_meta$attribute)
  
  nc_meta
}

# https://github.com/USGS-R/ncdfgeom/blob/main/R/read_timeseries_dsg.R#L64-L82
# NOT MODIFIED
check_timeseries_atts <- function(nc_atts) {
  
  if(!"name" %in% names(nc_atts)) {
    names(nc_atts) <- c("name", "variable", "value")
  }
  
  # Check important global atts
  check <- filter(nc_atts, .data$variable == "NC_GLOBAL" & .data$name == 'Conventions')$value
  if(length(check) == 0 || !grepl('CF', check)) {
    warning('File does not advertise CF conventions, unexpected behavior may result.') 
  }
  
  check <- filter(nc_atts, .data$variable == "NC_GLOBAL" & .data$name == "featureType")$value
  if(length(check) == 0 || !grepl('timeSeries', check)) {
    warning('File does not advertise use of the CF timeseries featureType, unexpected behavior may result.') 
  }
  
  nc_atts  
}

# https://github.com/USGS-R/ncdfgeom/blob/main/R/read_timeseries_dsg.R#L84-L152
# NOT MODIFIED except for references to pkg.env variables
get_dsg_meta <- function(nc, nc_meta = NULL) {
  
  if(is.null(nc_meta)) nc_meta <- get_nc_meta(nc)
  
  dsg <- list()
  
  dsg$timeseries_id <- get_timeseries_id(nc_meta$attribute)
  
  dsg$coord_vars <- get_coord_vars(nc)
  
  dsg$data_vars <- filter(nc_meta$attribute, .data$name == "coordinates" & 
                            grepl(paste(dsg$coord_vars, collapse = "|"), .data$value))
  
  # Given the coordinates found look for one and only one variable 
  # with standard name time, latitude, and longitude. 
  # OR (worst case maybe don't support) units like 'days since 1970-01-01 00:00:00', 
  # or 'degrees_east', or 'degrees_north'
  
  dsg$sn <- filter(nc_meta$attribute, .data$name == "standard_name")
  dsg$lat <- filter(dsg$sn, .data$value == "latitude") # pkg.env$lat_coord_var_standard_name
  dsg$lon <- filter(dsg$sn, .data$value == "longitude") # pkg.env$lon_coord_var_standard_name
  dsg$alt <- filter(dsg$sn, .data$value == "height") # pkg.env$alt_coord_var_standard_name
  dsg$time <- filter(dsg$sn, .data$value == "time") # pkg.env$time_var_standard_name
  
  if(nrow(dsg$time) == 0) {
    dsg$time <- filter(nc_meta$attribute, .data$name == "units" & grepl(" since ", .data$value))
  }
  
  if(length(dsg$lat) == 0) { stop('No latitude coordinate found.') }
  if(length(dsg$lon) == 0) { stop('No longitude coordinate found.') }
  if(length(dsg$time) == 0) { stop('No time coordinate found.') }
  
  dsg$dim_time <- filter(nc_meta$axis, .data$variable == dsg$time$variable)$dimension
  dsg$dim_tsid <- filter(nc_meta$axis, .data$variable == dsg$timeseries_id)$dimension
  
  if (nrow(dsg$data_vars) == 0) {
    warning("no data variables found, attempting to infer via shared dimensions")
    
    axes_search <- group_by(nc_meta$axis, .data$variable)
    axes_search <- filter(axes_search, sum(c(dsg$dim_tsid, dsg$dim_time) %in% .data$dimension) == 2 &
                            !.data$variable %in% c(dsg$coord_vars, dsg$timeseries_id))
    
    dsg$data_vars <- data.frame(variable = unique(axes_search$variable))
    
    if(nrow(dsg$data_vars) == 0) stop("No data variables could be identified")
  }
  
  dsg$var_meta <- list()
  
  for(data_var in dsg$data_vars$variable) {
    dsg$varmeta[[data_var]] <- list() 
    
    dsg$varmeta[[data_var]]$nc_var <- filter(nc_meta$variable, .data$name == data_var)
    
    # Ensures we get back data with time in rows.
    var_inq <- var.inq.nc(nc, data_var)
    if(var_inq$type == "NC_CHAR") {
      dsg$varmeta[[data_var]]$dims <- var_inq$dimids[var_inq$dimids %in% c(dsg$dim_time, dsg$dim_tsid)]
    } else {
      dsg$varmeta[[data_var]]$dims <- var_inq$dimids
    }
    
    dsg$varmeta[[data_var]]$dim_order <- match(dsg$varmeta[[data_var]]$dims, 
                                               c(dsg$dim_time, dsg$dim_tsid))
    
  }
  
  dsg
}

# MODIFIED to pull in depths and in read data section and read in timeseries ids
get_nc_list <- function(nc, dsg, nc_meta, read_data) {
  
  nc_list<-list()
  
  time_vals <- var.get.nc(nc, dsg$time$variable)
  time_units <- filter(nc_meta$attribute, .data$variable == dsg$time$variable & .data$name == "units")
  
  nc_list$time <- utcal.nc(time_units$value[[1]], time_vals, type = "c")
  
  if(nrow(dsg$lat) > 0) {
    nc_list$lats <- var.get.nc(nc, dsg$lat$variable)
  } else {
    warning("no latitude coordinate found")
    nc_list$lats <- numeric(0)
  }
  
  if(nrow(dsg$lon) > 0) {
    nc_list$lons <- var.get.nc(nc, dsg$lon$variable)
  } else {
    warning("no longitude coordinate found")
    nc_list$lons <- numeric(0)
  }
  
  if(nrow(dsg$alt) > 0) { 
    nc_list$alts <- var.get.nc(nc, dsg$alt$variable)
  } else {
    warning("no altitude coordinate found")
    nc_list$alts <- numeric(0)
  }
  
  if(nrow(dsg$depth) > 0) { 
    nc_list$depth <- var.get.nc(nc, dsg$depth$variable)
  } else {
    warning("no depth coordinate found")
    nc_list$depth <- numeric(0)
  }
  
  nc_list$timeseries_id <- as.character(var.get.nc(nc, dsg$timeseries_id))
  
  # For all variables that have a 'coordinates' attribute that matches the one found earlier...
  nc_list$varmeta <- list()
  
  for(data_var in dsg$data_vars$variable) {
    
    nc_list$data_unit[data_var] <- filter(nc_meta$attribute, .data$variable == data_var &
                                            .data$name == "units")$value[[1]]
    nc_list$data_prec[data_var] <- dsg$varmeta[[data_var]]$nc_var$type # todo map this to NetCDF types
    
    nc_list$varmeta[data_var][[1]]$name <- data_var
    nc_list$varmeta[data_var][[1]]$long_name <- filter(nc_meta$attribute, .data$variable == data_var &
                                                         .data$name == "long_name")$value[[1]]
    
  }
  
  if(read_data) {
    global_featureType <- filter(nc_meta$attribute, .data$variable == 'NC_GLOBAL' &
                                   .data$name == "featureType")$value[[1]]
    for(data_var in dsg$data_vars$variable) {
      nc_list$data_frames[data_var][[1]] <- as.data.frame(var.get.nc(nc, data_var))
      
      if (global_featureType == 'timeSeriesProfile') {
        var_featureType <- filter(nc_meta$attribute, .data$variable == data_var &
                                    .data$name == "featureType")$value[[1]]
      } else if (global_featureType == 'timeSeries') {
        var_featureType <- 'timeSeries'
      }
      
      if(dsg$varmeta[[data_var]]$dim_order[1] > dsg$varmeta[[data_var]]$dim_order[2])
        nc_list$data_frames[data_var][[1]] <- t(nc_list$data_frames[data_var][[1]])
      
      if (var_featureType == 'timeSeries') {
        colnames(nc_list$data_frames[data_var][[1]]) <- as.character(var.get.nc(nc, dsg$timeseries_id))
      } else if (var_featureType == 'timeSeriesProfile') {
        timeseries_ids <- as.character(var.get.nc(nc, dsg$timeseries_id))
        depth_length <- filter(nc_meta$dimension, .data$name == 'depth')$length
        depths <- as.character(var.get.nc(nc, dsg$profile_id))
        colnames(nc_list$data_frames[data_var][[1]]) <- paste0(rep(timeseries_ids, depth_length),'_', rep(depths, each=length(timeseries_ids)))
      }
    }
  }
  
  nc_list
}

# NEW FUNCTION
get_profile_id <- function(nc_atts) {
  # Look for variable with the profile_id in it.
  profile_id <- filter(nc_atts, .data$name == "cf_role" &
                         .data$value == "profile_id")$variable
  
  if(length(profile_id) == 0) { 
    stop('A profile id variable was not found in the file.') 
  }
  
  profile_id
}

# https://github.com/USGS-R/ncdfgeom/blob/main/R/read_timeseries_dsg.R#L212-L224
# NOT MODIFIED except for reference to `pkg.env$timeseries_id_cf_role`
get_timeseries_id <- function(nc_atts) {
  # Look for variable with the timeseries_id in it.
  timeseries_id <- filter(nc_atts, .data$name == "standard_name" &
                            .data$value == "station_id")$variable
  timeseries_id <- filter(nc_atts, .data$name == "cf_role" &
                            .data$value == "timeseries_id")$variable #pkg.env$timeseries_id_cf_role = 'timeseres_id'
  
  if(length(timeseries_id) == 0) { 
    stop('A timeseries id variable was not found in the file.') 
  }
  
  timeseries_id
}

# https://github.com/USGS-R/ncdfgeom/blob/main/R/read_timeseries_dsg.R#L226-L238
#NOT MODIFIED
get_coord_vars <- function(nc) {
  # Look for 'coordinates' that match variable names. 
  # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/ch09s05.html
  nc_coord_vars <- ncmeta::nc_coord_var(nc)
  
  if (nrow(nc_coord_vars) == 0) { 
    stop('No coordinates declarations were found in the file.') 
  }
  
  coord_vars <- unique(c(nc_coord_vars$X, nc_coord_vars$Y, 
                         nc_coord_vars$Z, nc_coord_vars$T))
  coord_vars[!is.na(coord_vars)]
}



# https://github.com/USGS-R/ncdfgeom/blob/main/R/read_timeseries_dsg.R#L242-L261
# Not MODIFIED
add_globals <- function(nc_list, nc_meta) {
  nc_list$global_attributes$nc_summary <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                   .data$name == "summary")$value
  nc_list$global_attributes$nc_date_created <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                        .data$name == "date_created")$value
  nc_list$global_attributes$nc_creator_name <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                        .data$name == "creator_name")$value
  nc_list$global_attributes$nc_creator_email <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                         .data$name == "creator_email")$value
  nc_list$global_attributes$nc_project <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                   .data$name == "project")$value
  nc_list$global_attributes$nc_proc_level <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                      .data$name == "processing_level")$value
  nc_list$global_attributes$nc_title <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                 .data$name == "title")$value
  
  attr(nc_list, "class") <- "ncdfgeom"
  
  nc_list
}