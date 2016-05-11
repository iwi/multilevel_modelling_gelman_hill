# 16.9 - Schools example

library(rstan)
library(arm)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## not sure if the schools data is available. The data below doesn't seem to be it.
schools <- read.table('~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/schools/schools.dat',
                      stringsAsFactors = F)
str(schools)
