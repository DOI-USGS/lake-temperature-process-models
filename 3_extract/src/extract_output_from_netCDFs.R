library(RNetCDF)
library(ncmeta) # need version 0.3.5 or higher - devtools::install_github("https://github.com/hypertidy/ncmeta.git")
library(tidyverse)
source('3_extract/src/netCDF_extract_utils.R')

nc_file <- '3_extract/out/Tallgrass/EALSTM_NLDAS_WI.nc' # '3_extract/out/Tallgrass/GLM_NLDAS_10k.nc' #

# Read in information about netCDF (variables, dates, etc.)
# Warning 'no altitude coordinate found' is expected
nc_info <- read_timeseries_profile_dsg(nc_file, read_data = FALSE) # read_data set to FALSE b/c netCDF is too large for all data to be read in at once

# Define lake sites of interest

# random selection
lake_sites <- c("nhdhr_109943476", "nhdhr_109943604", "nhdhr_109943606", "nhdhr_{00A0B20D-B53E-4189-AE14-461AAC64BD53}", 
                "nhdhr_114336399", "nhdhr_{08C38C24-CCB1-4DDB-97DB-8B8973AEE01D}", 
                "nhdhr_114336431", "nhdhr_114336515", "nhdhr_114336521", "nhdhr_114336965", 
                "nhdhr_{0416B77B-D03F-4C5B-B8A1-EAE9DA39A087}", "nhdhr_03ce2e4f-9689-46a6-817b-d1b303960399", 
                "nhdhr_106374138", "nhdhr_106374184", "nhdhr_106374206", "nhdhr_106374244", 
                "nhdhr_106374252", "nhdhr_106374254", "nhdhr_{02510024-2C0B-4F00-B866-7FBA96AE1EB4}", 
                "nhdhr_114336967")

# experimenting w/ fully and partially continuous site lists
lake_sites <- c(nc_info$timeseries_id[3:13], nc_info$timeseries_id[1], nc_info$timeseries_id[90:95], nc_info$timeseries_id[65:66]) #nc_info$timeseries_id[1:20]# 

# Open netCDF
nc <- open.nc(nc_file)

# Pull temperature predictions (for all dates and all depths) for those lakes.
# Depth units = meters, temperature units = degrees Celsius
temp_data <- pull_data_for_sites(nc, nc_info, var = 'temp', sites = lake_sites, long_format = FALSE)

# Pull boolean ice predictions (for all dates) for those lakes
# Ice units: 1 = ice is present; 0 = no ice is present
ice_data <- pull_data_for_sites(nc, nc_info, var = 'ice', sites = lake_sites, long_format = TRUE)

# close netCDF
close.nc(nc)
