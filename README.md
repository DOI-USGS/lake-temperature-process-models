# GLM modeling of lake temperatures

This repository is for running uncalibrated GLM models of lake temperatures.

-------------------
## Dependent files 
* GLM 3 template
  * `'1_prep/in/glm3_template.nml'` (committed to repo)
* NLDAS driver files (stored on Caldera)
  * _e.g._, `'1_prep/in/NLDAS_time[0.379366]_x[231]_y[167].csv'`

*Files  from [`lake-temperature-model-prep pipeline`](https://github.com/USGS-R/lake-temperature-model-prep) that will eventually be transferred using GLOBUS (location in `lake-temperature-model-prep` --> location in this pipeline):*
* List of lake-specific attributes for nml modification: `'7_config_merge/out/nml_list.rds'` --> `'1_prep/in/nml_list.rds'`
* Temperature observations: `'7b_temp_merge/out/merged_temp_data_daily.feather'` --> `'1_prep/in/merged_temp_data_daily.feather'`
* Lake-to-state crosswalk: `'2_crosswalk_munge/out/lake_to_state_xwalk.rds'` --> `'1_prep/in/lake_to_state_xwalk.rds'`
* Lake - GCM cell tile crosswalk: `'7_drivers_munge/out/lake_cell_tile_xwalk.csv'` --> `'1_prep/in/lake_cell_tile_xwalk.csv'`
  * Created within the [targets sub-pipeline](https://github.com/USGS-R/lake-temperature-model-prep/blob/main/_targets.R), look for the lake_cell_tile_xwalk_df target
* Munged GCM netCDF files (one per GCM): `'7_drivers_munge/out/GCM_{gcm name}.nc'` --> `'1_prep/in/GCM_{gcm name}.nc'`



## Running the pipeline on HPC, in parallel

### Tallgrass quickstart

``` r
ssh tallgrass.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/lake-temperature-process-models

# Change user permissions for collaboration.
# Best practice is to add this line to your `~/.bashrc` on tallgrass, so you don't forget!
umask 002
```

### Singularity

Singularity is a program for running code in containers. It is fundamentally similar to docker, and is capable of generating containers based on docker images. It is the containerization technology used in the tallgrass and yeti HPC environments. For more information, see [here](https://code.usgs.gov/wwatkins/hpc_container_blog).

For the following applications, you'll need to load the singularity and slurm modules:

``` bash
module load singularity slurm
```

Here's how to get the image that Jesse built from Dockerhub and translate it to Singularity (this has already been done for the image listed below, and should only need to be re-done if a new image is built):

``` bash
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/lake-temperature-process-models
singularity pull docker://jrossusgs/glm3r:v0.7.1

# Now you can see the singularity image: it is a file called glm3r_v0.7.1.sif.
# Create a symlink so that the launch-rstudio-container.slurm points to the new
# container.
rm glm3r.sif
ln -s glm3r_v0.7.1.sif glm3r.sif
```

**Running the pipeline in the Singularity container**

Here's how to build targets using the Singularity container and the `targets::tar_make_clustermq(target_name, workers=n_workers)` option to build targets in parallel within the Singularity container, with a specified number of workers (up to 72, as Tallgrass has 72 cores per node). The `srun` command will allocate a node and then run the specified command, in this case `Rscript`. Targets will then delegate work out to `n_workers` cores for any parallelizable step that you don't specifically tell it to run in serial.

``` r
# Build GCM-driven GLM models, in parallel
srun --pty -c 72 -t 7:00:00 -A watertemp singularity exec glm3r.sif Rscript -e 'targets::tar_make_clustermq(p2_gcm_glm_uncalibrated_runs, workers=60)'
```

``` r
# Build NLDAS-driven GLM models, in parallel
srun --pty -c 72 -t 1:00:00 -A watertemp singularity exec glm3r.sif Rscript -e 'targets::tar_make_clustermq(p2_nldas_glm_uncalibrated_runs, workers=60)'
```

**Running the pipeline interactively**

Here's how to run the Singularity container interactively on an allocated job:

``` r
srun --pty -c 72 -t 10:00:00 -A watertemp singularity exec glm3r.sif bash
R
library(targets)
tar_make_clustermq(p2_gcm_glm_uncalibrated_runs, workers=72)
# etc
```

**Example interactive workflow that launches model runs on Tallgrass**

In git bash window:
```bash
[hcorson-dosch@tg-login1 ~] ssh tallgrass.cr.usgs.gov
[hcorson-dosch@tg-login1 ~] cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/lake-temperature-process-models
[hcorson-dosch@tg-login1 lake-temperature-process-models] umask 002
[hcorson-dosch@tg-login1 lake-temperature-process-models] screen # set up screen so that if lose Pulse Secure connection, run continues
[hcorson-dosch@tg-login1 lake-temperature-process-models] module load singularity slurm
[hcorson-dosch@tg-login1 lake-temperature-process-models] srun --pty -c 72 -t 10:00:00 -A watertemp singularity exec glm3r.sif bash # Here I'm requesting 72 cores (1 node) for 10 hours
```
Once the resources have been allocated, you'll immediately be transferred to the allocated node, and will be in the container environment.

To access R, simply type `R`:

```bash
hcorson-dosch@ml-0008:/caldera/projects/usgs/water/iidd/datasci/lake-temp/lake-temperature-process-models$ R

R version 4.1.2 (2021-11-01) -- "Bird Hippie"
Copyright (C) 2021 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

>
```
Once in R, you could immediately launch the model runs (here, the NLDAS model runs):
```r
> library(targets) # load targets
> tar_make_clustermq(p3_nldas_glm_uncalibrated_output_zips, reporter='summary', workers=60) # To run the NLDAS models and extract and package the output
```
Or build other targets (e.g., the model configuration) before launching the model runs (here, the GCM model runs):
```r
> library(targets)
> tar_make_clustermq(p1_gcm_model_config, reporter='summary', workers=60) # Typically I build the config first so that I can check it before launching the model run - here I'm building the GCM model config
> tar_load(p1_gcm_model_config)
> tar_load(p1_site_ids)
> nrow(p1_gcm_model_config) == (length(p1_site_ids)*6*3) # Check # of model runs is correct, for GCMs thats # lakes * 6 gcms * 3 time periods
> Sys.time() # I find it helpful to have a console record of the time when I launch a run
> tar_make_clustermq(p2_gcm_glm_uncalibrated_runs, reporter='summary', workers=60) # To launch just the model runs
> tar_make_clustermq(p3_gcm_glm_uncalibrated_output_zips, reporter='summary', workers=50) # To launch the GCM model runs *and* extract and package the output
> library(tidyverse)
> tar_load(p2_gcm_glm_uncalibrated_runs)
> nrow(filter(p2_gcm_glm_uncalibrated_runs, glm_success==FALSE)) # check how many runs failed
> failed_runs <- p2_gcm_glm_uncalibrated_runs %>% filter(glm_success==FALSE) %>% group_by(site_id) %>% summarize(n_failed_runs = n()) # get summary of # of failed runs per lake
> nrow(failed_runs) # check how many lakes had failed runs
> tar_load(p2_gcm_glm_uncalibrated_run_groups) 
> length(unique(p2_gcm_glm_uncalibrated_run_groups$site_id)) # check for how many lakes all 18 runs (6 GCMs * 3 time periods) succeeded and therefore for how many lakes results will be extracted in 3_extract
```
_Note: I've been using a number of `workers` < 72 in my `tar_make_clustermq()` command (despite having an allocated node with 72 cores) because I noticed when calling `tar_make_clustermq()` with `workers=72` that the pipeline would sometimes hit an error: `Error in tar_throw_run(target$metrics$error) : Resource temporarily unavailable`, with warnings about 'unclean shutdown for PIDs', particularly when building the output feather files. It seems to runs more smoothly if you run `tar_make_clustermq()` with fewer workers than the number of available cores. For generating the output files I had to drop it to `workers = 50`._

**Editing the pipeline in RStudio on Tallgrass**

You can also get an interactive RStudio on tallgrass. The best documentation for this is currently [here](https://code.usgs.gov/wma/wp/pump-temperature#running-interactive-sessions-on-hpc). The tl;dr is

``` bash
# Launch the session
sbatch launch-rstudio-container.slurm
# Make sure the session is running on a compute node
squeue -u jross
# Now read the generated instructions for how to access the session
cat tmp/rstudio_jross.out
```

**_Caution_**

RStudio may not be as good an environment for running parallelized targets pipelines as running them through `Rscript -e`. The [clustermq user guide](https://cran.r-project.org/web/packages/clustermq/vignettes/userguide.html) says that the `multicore` scheduler sometimes causes problems in RStudio. I haven't run into this, but if it happens, you might need to switch to `multiprocess`. This uses more RAM. Might not be a problem, just something to be aware of!

------------------------------------------------------------------------

## Building the Docker image

This is as simple as editing the Dockerfile and running a command to rebuild it. What follows is a teaser. It won't be as simple as this, because currently the image is hosted on Jesse's Docker Hub. We should put the image on the CHS docker server instead, but we can wait until when (or, if) it needs to be built again to do so.

``` bash
cd docker
docker-compose build   # maybe change version tag in docker-compose.yml first
docker-compose up      # test it
docker-compose push    # push the updated image to the server
```

------------------------------------------------------------------------

## Running the pipeline locally, in serial

You can simply build targets as normal, using `tar_make()`, and `targets` will ignore the `cluster_mq.scheduler` options set in `'_targets.R'`

## Running the pipeline locally, in parallel

The pipeline can be run in parallel locally through docker, just as it can be run through Singularity on tallgrass.

Simple command-line R interface:

``` bash
docker pull jrossusgs/glm3r:v0.7.1
cd ~/lake-temperature-process-models
docker run -v '/home/jross/lake-temperature-process-models/:/lakes' -it jrossusgs/glm3r:v0.7.1 R
# Now you have an R prompt in the container, with the project directory mounted at `/lakes/`.
# You can `setwd("/lakes")` and start working.
```

Or alternatively, you could run RStudio in the container and access it through your browser (user is rstudio, password set in the startup command as mypass).

``` bash
docker pull jrossusgs/glm3r:v0.7.1
cd ~/lake-temperature-process-models
docker run -v '/home/jross/lake-temperature-process-models/:/lakes' -p 8787:8787 -e PASSWORD=mypass -e ROOT=TRUE -d jrossusgs/glm3r:v0.7.1
```

``` r
setwd("/lakes") 
# Do a lot of work at once and test your computer's fan
targets::tar_make_clustermq(p2_gcm_glm_uncalibrated_runs, workers = 32)
```

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


[
  ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
](http://creativecommons.org/publicdomain/zero/1.0/)
