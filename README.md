# GLM modeling of lake temperatures
This repository is for running uncalibrated GLM models of lake temperatures.

## Dependent files from [`lake-temperature-model-prep pipeline`](https://github.com/USGS-R/lake-temperature-model-prep)
_Files that will eventually be transferred using GLOBUS:_
  * lake - GCM cell crosswalk: `'1_prep/in/lake_cell_xwalk.csv'`
    * Created within [`gcm_driver_data_munge_pipeline` branch](https://github.com/USGS-R/lake-temperature-model-prep/tree/gcm_driver_data_munge_pipeline) of repo
  * list of lake-specific attributes for nml modification: `'1_prep/in/nml_list.rds'`
  * Munged GCM netCDF files (not yet brought in manually, see below)

_Files used in current testing development phase:_
  * In place of munged GCM netCDF files, manually bringing in feather files created by Lindsay for each GCM type, GCM cell, and time period: `'1_prep/tmp/GCM_{gcm name}_{gcm time period}_{gcm cell number}.feather'`

