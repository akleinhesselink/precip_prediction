#!/usr/bin/env Rscript

rm(list = ls() )

# -- select input files  -------------------------------------------------------------------#
args = commandArgs(trailingOnly=TRUE)

#args <- c('data/climate/all_clim_covs.csv', 'data/vital_rate/ARTR_growth.csv', 'data/temp_data/ARTR_survival.csv') # for testing 

gfile <- sfile <- rfile <- NULL

# test args
if (length(args)==3) {
  # default output file
  clim_file <- args[1]
  gfile <- args[2]
  sfile <- args[3]
}else if( length(args)==2){
  clim_file <- args[1]
  rfile <- args[2]
}else { 
  stop( 'Incorrect number of parameters provides.\n Provide location of climate data file and either location of growth and survival files\n or location of recruitment file')
}

source(file.path('code','prep_vital_rate_df_functions.R'))
clim_vars <- scan(file = file.path('data', 'climate', 'select_clim_vars.txt'), what = 'char')
clim <- scale_climate_vars(clim_file, clim_vars)

if(!is.null(gfile) & !is.null(sfile)){ 
  df <- clean_growth_survival(clim, gfile, sfile)
}else if(!is.null(rfile)){
  df <- clean_recruitment(clim, rfile)
}else{
  stop('too many files provided')
}

fname <- paste( unique(unlist(
  lapply(c(gfile, sfile, rfile), 
         function(x)
           {
            strsplit(sub('\\..*$', '', basename(x), perl= T), '_')
           }
         )
  )), collapse = '_')

saveRDS(df, file = file.path( 'data', 'temp_data', paste0(fname, '_dataframe.RDS')))
