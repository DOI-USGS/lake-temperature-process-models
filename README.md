# GLM modeling of lake temperatures
This repository is for running uncalibrated GLM models of lake temperatures.

-----------------
## Dependent files from [`lake-temperature-model-prep pipeline`](https://github.com/USGS-R/lake-temperature-model-prep)
_Files that will eventually be transferred using GLOBUS:_
  * Lake - GCM cell tile crosswalk: `'1_prep/in/lake_cell_tile_xwalk.csv'`
    * Created within [the `targets` sub-pipeline](https://github.com/USGS-R/lake-temperature-model-prep/blob/main/_targets.R), look for the `lake_cell_tile_xwalk_df` target
  * List of lake-specific attributes for nml modification: `'1_prep/in/nml_list.rds'`
  * Munged GCM netCDF files (not yet brought in manually, see below)

_Files used in current testing development phase:_
  * In place of munged GCM netCDF files, manually bringing in feather files created by Lindsay for each GCM type, GCM cell, and time period: `'1_prep/tmp/GCM_{gcm name}_{gcm time period}_{gcm cell number}.feather'`
  
-----------------

## Running the pipeline on HPC, in parallel

### Tallgrass quickstart
```R
ssh tallgrass.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/lake-temperature-process-models

# Change user permissions for collaboration.
# Best practice is to add this line to your `~/.bashrc` on tallgrass, so you don't forget!
umask 002
```

### Singularity
Singularity is a program for running code in containers. It is fundamentally similar to docker, and is capable of generating containers based on docker images. It is the containerization technology used in the tallgrass and yeti HPC environments. For more information, see [here](https://code.usgs.gov/wwatkins/hpc_container_blog).

For the following applications, you'll need to load the singularity and slurm modules:

```bash
module load singularity slurm
```
Here's how to get the image that Jesse built from Dockerhub and translate it to Singularity (this has already been done for the image listed below, and should only need to be re-done if a new image is built):
```bash
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/lake-temperature-process-models
singularity pull docker://jrossusgs/glm3r:v0.7
# now you can see the singularity image: it is a file called glm3r_v0.7.sif
```
Here's how to build targets using the Singularity container and the `targets::tar_make_clustermq(target_name, workers=n_workers)` option to build targets in parallel within the Singularity container, with a specified number of workers (up to 72, as Tallgrass has 72 cores per node). The `srun` command will allocate a node and then run the specified command, in this case `Rscript`. Targets will then delegate work out to `n_workers` cores for any parallelizable step that you don't specifically tell it to run in serial.
```R
srun --pty -c 10 -A watertemp singularity exec glm3r_v0.7.sif Rscript -e 'targets::tar_make_clustermq(p2_glm_uncalibrated_runs, workers=10)'
```

Here's how to run the Singularity container interactively on an allocated job:
```R
srun --pty -c 10  -A watertemp singularity exec glm3r_v0.7.sif bash
R
library(targets)
tar_make_clustermq(p2_glm_uncalibrated_runs, workers=79)
# etc
```

You can also get an interactive RStudio on tallgrass. The best documentation for this is currently [here](https://code.usgs.gov/wma/wp/pump-temperature#running-interactive-sessions-on-hpc). The tl;dr is

```bash
# Launch the session
sbatch launch-rstudio-container.slurm
# Make sure the session is running on a compute node
squeue -u jross
# Now read the generated instructions for how to access the session
cat tmp/rstudio_jross.out
```

### Caution
RStudio may not be as good an environment for running parallelized targets pipelines as running them through `Rscript -e`. The [clustermq user guide](https://cran.r-project.org/web/packages/clustermq/vignettes/userguide.html) says that the `multicore` scheduler sometimes causes problems in RStudio. I haven't run into this, but if it happens, you might need to switch to `multiprocess`. This uses more RAM. Might not be a problem, just something to be aware of!

-----------------

## Building the Docker image

This is as simple as editing the Dockerfile and running a command to rebuild it. What follows is a teaser. It won't be as simple as this, because currently the image is hosted on Jesse's Docker Hub. We should put the image on the CHS docker server instead, but we can wait until when (or, if) it needs to be built again to do so.

```bash
cd docker
docker-compose build   # maybe change version tag in docker-compose.yml first
docker-compose up      # test it
docker-compose push    # push the updated image to the server
```

-----------------
## Running the pipeline locally, in serial
You can simply build targets as normal, using `tar_make()`, and `targets` will ignore the `cluster_mq.scheduler` options set in `'_targets.R'`

## Running the pipeline locally, in parallel
The pipeline can be run in parallel locally through docker, just as it can be run through Singularity on tallgrass.

Simple command-line R interface:
```bash
docker pull jrossusgs/glm3r:v0.7
cd ~/lake-temperature-process-models
docker run -v '/home/jross/lake-temperature-process-models/:/lakes' -it jrossusgs/glm3r:v0.7 R
# Now you have an R prompt in the container, with the project directory mounted at `/lakes/`.
# You can `setwd("/lakes")` and start working.
```

Or alternatively, you could run RStudio in the container and access it through your browser (user is rstudio, password set in the startup command as mypass).
```bash
docker pull jrossusgs/glm3r:v0.7
cd ~/lake-temperature-process-models
docker run -v '/home/jross/lake-temperature-process-models/:/lakes' -p 8787:8787 -e PASSWORD=mypass -e ROOT=TRUE -d jrossusgs/glm3r:v0.7
```

```r
setwd("/lakes") 
# Do a lot of work at once and test your computer's fan
targets::tar_make_clustermq(p2_glm_uncalibrated_runs, workers = 32)
```
