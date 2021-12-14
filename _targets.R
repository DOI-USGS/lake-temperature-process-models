library(targets)

suppressPackageStartupMessages(library(tidyverse))
tar_option_set(packages = c('tidyverse', 'ncdf4', 'ncdf4.helpers', 'arrow'))

source('1_prep.R')
source('2_run.R')

# Return the complete list of targets
c(p1, p2)