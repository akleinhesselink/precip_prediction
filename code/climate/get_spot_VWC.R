#!/usr/bin/env Rscript

# calculate treatment effects compared to control soil moisture 

rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(lme4)
library(zoo)
library(MASS)

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=3) {
  stop("Supply location of 'driversdata' directory, season table and daily_VWC file", call.=FALSE)
} else if (length(args)==3) {
  # default output file
  drivers_dir <- args[1]
  season_tab <- args[2]
  daily_vwc_file <- args[3]
}

dataDir1 <- file.path(drivers_dir, 'data', 'idaho_modern', 'soil_moisture_data', 'data', 'processed_data')

# import soil moisture and climate data from driversdata
myVWC <- readRDS(file.path(dataDir1, 'decagon_data_with_station_data.RDS'))
daily_clim <- readRDS(file.path(dataDir1, 'daily_station_dat_rainfall.RDS'))
spotVWC <- readRDS(file.path(dataDir1, 'spring_spot_measurements.RDS'))

# local project data ----------------------------------------------------- # 
seasons <- read.csv(season_tab)
swVWC <- read.csv(daily_vwc_file) # soil wat output file exported by climate/ExtractData_3Runs.R, requires SoilWat package
climate_dir <- dirname(daily_vwc_file)

# ------------------------------------------------------------------------- # 

spotVWC <- 
  spotVWC %>% 
  mutate( month = as.numeric( strftime( date, '%m'))) %>% 
  left_join(seasons, by = 'month')

spot_weights <- 
  spotVWC %>% 
  group_by( date, PrecipGroup ) %>% 
  summarise( weight = n())

spotVWC <- merge( spotVWC, daily_clim [ , c('date', 'rainfall')])

spotVWC <- 
  spotVWC %>% 
  group_by( season, date, PrecipGroup,rainfall, Treatment ) %>% 
  summarise( avg_VWC = mean(VWC, na.rm = TRUE)) %>% 
  group_by(PrecipGroup) %>% 
  mutate( avg_VWC = scale(avg_VWC, mean(avg_VWC[Treatment == 'Control'], na.rm = T), sd(avg_VWC[Treatment == 'Control'], na.rm = T))) %>%  # scale within Precip Group and Depth 
  spread( Treatment, avg_VWC) %>%
  mutate( Drought = Drought - Control, Irrigation = Irrigation - Control ) %>% 
  arrange( PrecipGroup, date) 

spotVWC <- merge( spotVWC, spot_weights)

spotVWC <- spotVWC %>% mutate( simple_date = date ) 

write.csv(spotVWC, file.path( climate_dir, 'spot_VWC.csv'), row.names = F)
