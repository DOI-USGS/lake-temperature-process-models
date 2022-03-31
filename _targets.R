library(targets)
options(clustermq.scheduler = "multicore")

suppressPackageStartupMessages(library(tidyverse))
tar_option_set(packages = c('tidyverse', 
                            'data.table',
                            'ncdfgeom',
                            'arrow',
                            'lubridate',
                            'ggplot2'))

source('1_prep.R')
source('2_run.R')
source('3_extract.R')
source('4_visualize.R')

# Return the complete list of targets
c(p1, p2, p3, p4)
