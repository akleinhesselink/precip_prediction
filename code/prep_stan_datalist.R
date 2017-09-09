# prepare_data_lists_for_Stan.R

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(stringr)

# -- select covariates -------------------------------------------------------------------#
args = commandArgs(trailingOnly=TRUE)

#args <- c('HECO', 'recruitment', 'data/temp_data') # for testing 

if (length(args)!=3) {
  stop("Supply prepped dataframe file", call.=FALSE)
} else if (length(args)==3) {
  # default output file
  spp <- args[1]
  vr  <- args[2]
  temp_dir <- args[3]
}

source(file.path('code', 'prep_stan_datalist_functions.R'))

file <- dir(temp_dir, paste0( spp, '.*', vr, '.*_dataframe'), full.names = T )
dat  <- readRDS(file)

if(vr == 'growth'){
  gs_dat <- make_growth_and_survival_datlist(dat)
  survival <- gs_dat[[1]]
  growth   <- gs_dat[[2]]
  out   <- add_survival_data(growth, survival)
}else if(vr == 'survival'){
  out <- make_growth_and_survival_datlist(dat)
  out <- out[[1]]
}else if(vr == 'recruitment'){
  out <- make_recruitment_datlist(dat)
}else { stop('vital rate supplied must be one of growth, survival or recruitment')}

saveRDS(out, file.path(temp_dir, paste(spp, vr, 'datalist_for_stan.RDS', sep = '_')))
