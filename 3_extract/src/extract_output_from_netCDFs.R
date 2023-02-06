library(RNetCDF)
library(ncmeta) # need version 0.3.5 or higher - devtools::install_github("https://github.com/hypertidy/ncmeta.git")
library(tidyverse)
source('3_extract/src/netCDF_extract_utils.R')

nc_file <- '3_extract/out/lake_temp_preds_glm_gcm/lake_temp_preds_glm_gcm_ACCESS_MN.nc' #'3_extract/out/Tallgrass/GLM_NLDAS_10k.nc' #'3_extract/out/Tallgrass/EALSTM_NLDAS_WI.nc' # 

# Read in information about netCDF (variables, dates, etc.)
# Warning 'no altitude coordinate found' is expected
nc_info <- read_timeseries_profile_dsg(nc_file, read_data = FALSE) # read_data set to FALSE b/c netCDF is too large for all data to be read in at once

# Define lake sites of interest
lake_sites <- c(nc_info$timeseries_id[3:13], nc_info$timeseries_id[1], nc_info$timeseries_id[90:95], nc_info$timeseries_id[65:66])

# Pull temperature predictions (for all dates and all depths) for those lakes.
# Can be slow to run for a large # of sites at once
# Depth units = meters, temperature units = degrees Celsius
# can specify wide (long_format = FALSE) or long format (long_format = TRUE)
# if wide format, columns are named {site_id}_{depth}
temp_data <- pull_data_for_sites(nc_file, nc_info, var = 'temp', sites = lake_sites, long_format = TRUE)

# Pull boolean ice predictions (for all dates) for those lakes
# Ice units: 1 = ice is present; 0 = no ice is present
# can specify wide (long_format = FALSE) or long format (long_format = TRUE)
# if wide format, columns are named {site_id}
ice_data <- pull_data_for_sites(nc_file, nc_info, var = 'ice', sites = lake_sites, long_format = TRUE)
