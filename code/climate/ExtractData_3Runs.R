#!/usr/bin/env Rscript

# -- select covariates -------------------------------------------------------------------#
args = commandArgs(trailingOnly=TRUE)

#args <- c('HECO', 'recruitment', 'data/temp_data') # for testing 
#args <- c('data/SW_files/sw_output.RData', 'data/climate/daily_VWC.csv')

if (length(args)!=2) {
  stop("Supply name of soilwat data file and output filename", call.=FALSE)
} else if (length(args)==2) {
  # default output file
  soilwat_file <- args[1]
  out_file <- args[2]
}

#This is a R script on how to extract data from the 3_Runs Folder
#The 3_Runs folder holds numerous subfolders - one for each site
#In each site subfolder there should be two files - sw_input.RData and sw_output.RData
#The sw_input file is all the input files that were given to the model
#The sw_output file is all the daily raw data. Includes water balance components (i.e. transpiration, evaporation) and water output (volumetric water content and soil water potential)
#### BEWARE - there are empty or defenct 'slots'. Do not be alarmed
library(DBI)
library(RSQLite)
# If necessary install Rsoilwat from GitHub : 
#system2(command = "git", args = "clone -b master --single-branch --recursive https://github.com/Burke-Lauenroth-Lab/Rsoilwat.git Rsoilwat")
# checkout version 1.1.4 
#system('cd Rsoilwat; git checkout tags/v1.1.4;')
# install the package 
#tools::Rcmd(args = paste("INSTALL Rsoilwat"))
library(Rsoilwat31) 

#Step 1 - Set working directory
#dir.prj <- soilwat_dir

#Step 2- load and save files
#Input
#load(file.path('~/Desktop/Results2/1_SheepStation_SheepStation1', "sw_input.RData"),verbose=TRUE)
#swInput<-swRunScenariosData[[1]]
#swInput@soils

#Structure of Data
#Input
# slotNames(swInput)
# swInput@cloud

#Output
load(soilwat_file, verbose =T) #"sw_output.RData"),verbose=T)

#NOTE - the number (X) in the runData object (so runData[[X]]) refers to different scenario ids
#So for example extracting runData[[1]] grabs the current scenario data
#For the 'historical' runs and the 'current' runs there will only be the current scenario data
#For the Future climate runs there will be numerous scenarios  (37 to be precise))
#To know what scenario the number represents you need to refer back to the scenario_labels table in either the Weather sqlite database OR the Aggregated ouptut database
#Also please note that the 'years' in the future scenarios are broken. They reflect 'the current time period' but rest assured the data was generated with future climate data.

swOutputCurrent <- runData[[1]]

#swOutputFuture<-runData[[37]]#will only work if you have future data

#Further definitions of outputs are in the 'outsetup_v31.in' file
#slotNames(swOutputFuture)
# str(swOutputFuture@VWCMATRIC)
# str(swOutputFuture@VWCMATRIC@Day)
# 

VWC <-data.frame(swOutputCurrent@VWCMATRIC@Day)

VWC <- subset(VWC, !(Year == 2016 & DOY >= 270))

write.csv(VWC, out_file, row.names = FALSE)
