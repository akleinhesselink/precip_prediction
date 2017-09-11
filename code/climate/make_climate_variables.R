#!/usr/bin/env Rscript

#######################################################################################
#
# Setup seasonal climate variables for demographic rate models 
#
#######################################################################################

rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(lme4)
library(zoo)
library(stringr)

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=3) {
  stop("Supply location of 'driversdata' directory, season table, and plot theme", call.=FALSE)
} else if (length(args)==3) {
  # default output file
  input_dir <- args[1]
  season_tab <- args[2]
  figure_theme <- args[3]
}

# ----- read in drivers data data ------------------------------------------------------------#
dataDir1 <- file.path(input_dir, 'data', 'idaho_modern', 'climateData')

old_climate_files <- dir( dataDir1, pattern = 'Zachman', full.names = T) # Climate Data from Ecological Archives 
old_station_dat <- lapply(old_climate_files, read.csv)
station_dat <- read.csv(file.path(dataDir1, 'USSES_climate_monthly_new.csv'))

# ----- local data -------------------------------------------- # 
load(figure_theme)
season <- read.csv(season_tab)

# output dir------------------ 
climate_dir <- dirname(season_tab)

# --------------------------------------------------------------- # 

dm <- c(4, 11 ) # Drought months 
im <- c(5, 10 ) # Irrigation months

p.treatments <- c(0.5, 1.5) # Drought and Irrigation adjustments to precip 
t.treatments <- c(1, 1)     # Drought and Irrigation adjustments to temperature 

# make time periods --------------------------------------------------------------------

p1 <- data.frame( Period = 'Modern', year = 2007:2016)
p2 <- data.frame( Period = 'not monitored', year = 1958:2006)
p3 <- data.frame( Period = 'Historical', year = 1925:1957)
periods <- data.frame( rbind( p1, p2, p3 )) 

# --------------------------------------------------------------- # 

old_station_dat[[1]] <- old_station_dat[[1]] %>% 
  dplyr::select( - ANNUAL) %>% 
  gather(Month_name, TAVG, JAN:DEC) %>% 
  mutate( TAVG = (TAVG - 32)*5/9) # convert to celsius 

old_station_dat[[2]] <- old_station_dat[[2]] %>% 
  dplyr::select( - ANNUAL) %>% 
  gather(Month_name, PRCP, JAN:DEC) %>% 
  mutate( PRCP = PRCP*25.4)       # convert to mm 

months <- data.frame( month = 1:12, Month_name = toupper( month.abb))

old_station_dat <- merge( old_station_dat[[1]], old_station_dat[[2]], by = c('YEAR', 'Month_name'))
old_station_dat <- merge( old_station_dat, months)

# ---process dates----------------------------------------------------------------------#
old_station_dat$year <- as.numeric(old_station_dat$YEAR)

station_dat <- station_dat %>% separate(DATE, c('year', 'month'), '-') %>% mutate(month = as.numeric(as.character(month)), year = as.numeric(as.character(year)))

# set-up aggregate seasonal variables for model ----------------------------------------#
month_data <- merge(  old_station_dat[ , c('year', 'month', 'PRCP', 'TAVG')], station_dat[, c('year', 'month', 'PRCP', 'TAVG')], by = c('year', 'month'), all = TRUE) 

# take care of missing values 
month_data <- 
  month_data %>% 
  mutate( TAVG = ifelse(is.na(TAVG.x), TAVG.y, TAVG.x)) %>% 
  mutate( PRCP = ifelse(is.na(PRCP.x), PRCP.y, PRCP.x))

df <- merge( month_data, season, by = 'month')
  
df <- df %>% mutate( water_year = year + lag_year ) %>% 
  mutate( quarter = cut(month, 4, labels = paste0('Q', 1:4))) %>%
  dplyr::select(year, quarter, month, year, season, season_label, precip_seasons, water_year, PRCP, TAVG)

# ---------- annual average Temperature -------------------------------------------------#
annual_MAT <- 
  df %>% 
  group_by( year ) %>%
  summarise (MAT = mean(TAVG, na.rm = TRUE))

# ---------- annual total precip --------------------------------------------------------#
annual_TPPT <- 
  df %>% 
  group_by( year ) %>% 
  summarise( TPPT = mean(PRCP, na.rm = TRUE), n = n())

# ---------- seasonal average Temperature -----------------------------------------------#
seasonal_tmean <- 
  df %>% 
  mutate(year = ifelse(month == 12 , year + 1, year  )) %>% # account for December 
  group_by(year, season_label) %>% 
  summarise( l0 = mean(TAVG, na.rm = TRUE) )

seasonal_tmean <- 
  seasonal_tmean %>% 
  ungroup() %>% 
  group_by(season_label)%>% 
  arrange( season_label, year ) %>% 
  mutate( l1 = lag ( l0, 1 ), 
          l2 = lag ( l0, 2 ) ) %>% 
  gather( lag, TAVG, l0:l2 ) %>% 
  ungroup( ) %>% 
  unite( stat,  season_label, lag , sep = '_TMEAN_') %>% 
  spread( stat, TAVG )

# ---------- monthly climate  -----------------------------------------------------------#
# 
# incorporate Drought and Irrigation effects only in specificied months 
# 

monthly_clim <- 
  df %>% 
  mutate(Control = PRCP) %>% 
  dplyr::select(-PRCP) %>% 
  mutate( Drought    = ifelse( year > 2011 & month %in% c(dm[1]:dm[2]), Control*p.treatments[1], Control) ) %>% 
  mutate( Irrigation = ifelse( year > 2011 & month %in% c(im[1]:im[2]), Control*p.treatments[2], Control) ) %>% 
  gather(Treatment, PRCP, Control, Drought, Irrigation)


month_t <- 
  monthly_clim %>% 
  dplyr::select( year, month, TAVG ) %>% 
  distinct() %>% 
  spread( month , TAVG )


mydata <- month_t[, 2:13]
mydata <- mydata[complete.cases(mydata), ]
pca <- princomp(mydata)
biplot(pca)

# ------------ aggregate monthly climate with Treatment effects by quarter ---------------#

quarterly_clim <-
  monthly_clim %>% 
  gather( var, val , TAVG, PRCP ) %>% 
  group_by(Treatment, var, month) %>% 
  mutate( val = ifelse(is.na(val), mean(val, na.rm = TRUE), val)) %>% # !!!!!!! fill in missing monthly averages after 1925 with monthly average !!!!!!!! 
  group_by( Treatment, var, year, quarter ) %>%                       # !!!!!!! note missing values TAVG  !!!!!!!!
  summarise( avg = mean(val), ttl = sum(val) ) %>% 
  group_by(var) %>% 
  gather( stat, val, avg, ttl ) %>% 
  filter( (var == 'PRCP' & stat == 'ttl')| (var == 'TAVG' & stat == 'avg')) %>% 
  group_by( Treatment, var, stat) %>% 
  arrange(year, quarter) %>%
  ungroup() %>% 
  unite(var, c(var, stat) , sep = '_')

# -------------------------------------------------------------------------------------------#
# -------------- aggregate monthly climate with Treatment effects by season -----------------#
seasonal_clim <-
  monthly_clim %>% 
  mutate(year = ifelse(month == 12 , year + 1, year  )) %>% # account for December 
  gather( var, val , TAVG, PRCP ) %>% 
  group_by(Treatment, var, month) %>% 
  mutate( val = ifelse(is.na(val), mean(val, na.rm = TRUE), val)) %>% # !!!!!!! fill in missing monthly averages after 1925 with monthly average !!!!!!!! 
  group_by( Treatment, var, year, season ) %>%                       # !!!!!!! note missing values TAVG  !!!!!!!!
  summarise( avg = mean(val), ttl = sum(val) ) %>% 
  group_by(var) %>% 
  gather( stat, val, avg, ttl ) %>% 
  filter( (var == 'PRCP' & stat == 'ttl')| (var == 'TAVG' & stat == 'avg')) %>% 
  group_by( Treatment, var, stat) %>% 
  arrange(year, season) %>%
  ungroup() %>% 
  unite(var, c(var, stat) , sep = '_')

seasonal_precip <- 
  monthly_clim %>% 
  dplyr::select ( -TAVG) %>%  
  group_by( Treatment, water_year, precip_seasons ) %>% 
  summarise( PRCP = sum(PRCP) ) %>% 
  group_by( Treatment, precip_seasons ) %>% 
  arrange( water_year, precip_seasons) %>%
  rename( year = water_year ) %>% 
  arrange( year ) %>% 
  rename( l0 = PRCP) %>% 
  mutate( l1 = lag (l0, 1 ) , 
          l2 = lag (l0, 2 ) ) %>% 
  gather( lag, PRCP, l0:l2) %>% 
  ungroup() %>% 
  unite( stat , precip_seasons, lag , sep = '_PRCP_') %>% 
  spread( stat, PRCP )

# -------------- join dfs for variables -------------------------------------------------------#
#seasonal_clim <- left_join( seasonal_tmean, seasonal_precip, by = 'year') 

annual_clim <- left_join( annual_TPPT, annual_MAT)

seasonal_clim <- left_join( annual_clim, seasonal_clim)

# -------join periods -------------------------------------------------------------------------#

seasonal_clim <- left_join( seasonal_clim, periods )
monthly_clim <- left_join( df, periods ) 
quarterly_clim <- left_join( quarterly_clim, periods ) 
annual_clim <- left_join( annual_clim, periods ) 

# ------ filter out treatments ----------------------------------------------------------------# 
# seasonal_clim <- seasonal_clim %>% 
#   filter( !(year < 2011 & Treatment != 'Control')) # remove all Drought and Irrigation treatments prior to 2011 

# quarterly_clim <- quarterly_clim %>% 
#   filter( !(year < 2012 & Treatment != 'Control'))

# --------monthly from daily ------------------------------------------------------------------# 

# aggregate daily to monthly for Peter

station_dat_daily <- read.csv(file.path( dataDir1, 'USSES_climate.csv'))
station_dat_daily$date <- as.POSIXct( strptime( station_dat_daily$DATE, format = '%Y%m%d', tz = 'MST')    )

station_dat_daily <- station_dat_daily %>% dplyr::select( date, STATION, STATION_NAME, PRCP, TMAX, TMIN )  

station_dat_daily$year <- strftime(station_dat_daily$date, '%Y')
station_dat_daily$DOY <- as.numeric( strftime( station_dat_daily$date, '%j'))
station_dat_daily$month <- strftime( station_dat_daily$date, '%m')

station_dat_daily[ station_dat_daily == -9999] <- NA

monthly_from_daily <-
  station_dat_daily %>% 
  group_by ( year, month ) %>% 
  summarise( TMEAN = mean((TMAX + TMIN)/2, na.rm = TRUE), TPPT = sum(PRCP, na.rm = TRUE), ndays_missing_ppt = sum(is.na(PRCP)), n_days_missing_tmean = sum(is.na(TMAX + TMIN))) %>% 
  mutate( TMEAN = round(TMEAN, 1), TPPT = round(TPPT, 1))

# -------- output -----------------------------------------------------------------------------#
plot(data = subset(seasonal_clim, var == 'TAVG_avg' & Treatment == 'Control' & season == 'summer'), val ~ year)
plot(data = subset(seasonal_clim, var == 'TAVG_avg' & Treatment == 'Control' & season == 'fall'), val ~ year)

write.csv( seasonal_clim, file.path( climate_dir, 'seasonal_climate.csv'), row.names = F)
write.csv( monthly_clim, file.path( climate_dir, 'monthly_climate.csv'), row.names = F) 
write.csv( quarterly_clim, file.path( climate_dir, 'quarterly_climate.csv'), row.names = F)
write.csv( annual_clim, file.path( climate_dir, 'annual_climate.csv'), row.names = F)

# what was this for? 
# write.csv(monthly_clim %>% arrange( year, month) %>% rename(TPPT = PRCP, TMEAN = TAVG), file.path( climate_dir, 'monthly_climate.csv'), row.names = FALSE)

write.csv(monthly_from_daily, file.path( climate_dir, 'monthly_climate_from_from_daily.csv'), row.names = FALSE)

