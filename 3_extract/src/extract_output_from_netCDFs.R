library(RNetCDF)
library(ncmeta) # need version 0.3.5 or higher - devtools::install_github("https://github.com/hypertidy/ncmeta.git")
library(tidyverse)
source('3_extract/src/netCDF_extract_utils.R')

nc_file <- '3_extract/out/Tallgrass/GLM_NLDAS_5k.nc' #GLM_NLDAS.nc'

# for SMALL netCDF, can read directly w/ read_timeseries_profile_dsg
# nc_raw_data <- read_timeseries_profile_dsg(nc_file, read_data=TRUE)
# 
# ice_data <- nc_raw_data$data_frames$ice %>% 
#   mutate(time = nc_raw_data$time, .before = 1) %>% 
#   pivot_longer(cols = starts_with('nhdhr'), names_to='site_id', values_to = 'ice') %>% 
#   arrange(site_id, time)
# temp_data <- nc_raw_data$data_frames$temp %>% 
#   mutate(time = nc_raw_data$time, .before = 1) %>% 
#   pivot_longer(cols=(-time), names_to=c('site_id','depth'), names_pattern = '(.*)_(.*)', values_to='temperature') %>% 
#   mutate(depth = as.numeric(depth)) %>% 
#   arrange(site_id, time)
# temp_data_filtered <- temp_data %>% filter(!(is.na(temperature)))


nc_info <- read_timeseries_profile_dsg(nc_file, read_data=FALSE)
nc <- open.nc(nc_file)

#temp
data_var <- 'temp'
time_subset <- nc_info$time[1:365]
timeseries_ids <- nc_info$timeseries_id
timeseries_ids_subset <- timeseries_ids[1:2]
depths <- nc_info$depth
temp_subset <- as.data.frame(var.get.nc( nc, data_var, start=c(1, 1, 1), count= c(length(time_subset), length(timeseries_ids_subset), length(depths))))
colnames(temp_subset) <- paste0(rep(timeseries_ids_subset, length(depths)),'_', rep(depths, each=length(timeseries_ids_subset)))
temp_data <- temp_subset %>% 
  mutate(time = time_subset, .before = 1) %>% 
  pivot_longer(cols=(-time), names_to=c('site_id','depth'), names_pattern = '(.*)_(.*)', values_to='temperature') %>% 
  mutate(depth = as.numeric(depth)) %>% 
  arrange(site_id, time)
temp_data_filtered <- temp_data %>% filter(!(is.na(temperature)))

# ice
data_var <- 'ice'
time_subset <- nc_info$time
timeseries_ids_subset <- nc_info$timeseries_id
depths <- nc_info$depth
ice_subset <- as.data.frame(var.get.nc( nc, data_var, start=c(1, 1, 1), count= c(length(time_subset), length(timeseries_ids_subset), length(depths))))
colnames(ice_subset) <- as.character(timeseries_ids_subset)
ice_data <- ice_subset %>% 
  mutate(time = time_subset, .before = 1)

close.nc(nc_file)

# only run if dataset is subset of full dataset
# ice_long <- ice_data %>% 
#   pivot_longer(cols = starts_with('nhdhr'), names_to='site_id', values_to = 'ice') %>%
#   arrange(site_id, time)
