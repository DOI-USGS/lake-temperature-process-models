library(targets)
library(tarchetypes)
options(clustermq.scheduler = "multicore")

suppressPackageStartupMessages(library(tidyverse))
tar_option_set(packages = c('tidyverse', 
                            'data.table',
                            'ncdfgeom', # You need >= v1.1.2
                            'RNetCDF',
                            'arrow',
                            'lubridate',
                            'ggplot2'))

source('1_prep.R')
source('2_run.R')
source('3_extract.R')
source('4_visualize.R')
source('5_evaluate.R')

# Return the complete list of targets
c(p1, p2, p3, p4, p5)
