#!/usr/bin/env Rscript

# PBA March 2016
# modified by ARK July 2016
# call from precip_analysis_wrapper.r

#rm(list = ls())

#########################################
#  1. Import data, merge treatment effects and save out 
#########################################
source('code/get_vital_rate_functions.R')

args = commandArgs(trailingOnly=TRUE)
#args = c('~/driversdata', 'ARTR', 'recruitment')
# test if there is at least one argument: if not, return an error
if (length(args)!=3) {
  stop("Supply location of 'driversdata' directory, species name and vital rate", call.=FALSE)
} else if (length(args)==3) {
  # default output file
  input_dir <- args[1]
  doSpp <- args[2]
  doVR <- args[3]
}

sppList=c("ARTR","HECO","POSE","PSSP", "allcov", "allpts")
dataDir1 <- file.path(input_dir, 'data', 'idaho')
dataDir2 <- file.path(input_dir, 'data', 'idaho_modern')

#nonCompLength.s=5 #Number of columns in SppData that are not measures of competitors 

output_dir  <- file.path('data', 'vital_rate')
output_file <- paste0(doSpp, '_', doVR, '.csv')

if(doVR == 'recruitment'){
  out <- process_recruit(dataDir1, dataDir2, doSpp, doVR, sppList)
}else if( doVR %in% c('growth', 'survival')){
  out <- process_surv_grow(dataDir1, dataDir2, doSpp, doVR, sppList)
}else{ stop('incorrect vital rate name provided. Must be one of "growth", "survival", "recruitment"')}

# ----------- use this data for prediction ------------------------------------------------------------------------------
out <- out[ !out$Treatment %in% c('No_grass', 'No_shrub'), ]

# assign indicator variables -------------------------------------------------------------------------------- # 
out$Treatment2 <- out$Treatment
out$Treatment2[out$year>2000] <- "Modern"
out$Treatment3 <- out$Treatment
out$Treatment3[out$Treatment=="Control" & out$year>2000] <- "ControlModern"


write.csv(out, file.path(output_dir, output_file), row.names = F) 

# -----------------------------------------------------------------------------------------------------------------------

