#######################################################################################
#
# Setup seasonal SoilWAT variables for demographic rate models 
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
if (length(args)!=1) {
  stop("Supply location of climate directory", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  climate_dir <- args[1]
}

load('figures/my_plotting_theme.Rdata')

# ----- read in data --------------------------------------------------------------------#
VWC <- read.csv(file.path(climate_dir, 'daily_swVWC_treatments.csv'))

# make time periods --------------------------------------------------------------------

p1 <- data.frame( Period = 'Modern', year = 2007:2016)
p2 <- data.frame( Period = 'not monitored', year = 1958:2006)
p3 <- data.frame( Period = 'Historical', year = 1925:1957)
periods <- data.frame( rbind( p1, p2, p3 )) 


# set-up aggregate seasonal variables for model ----------------------------------------#
df <- 
  VWC %>% 
  ungroup() %>% 
  mutate( water_year = year + lag_year ) %>% 
  mutate( quarter = cut(month, 4, labels = paste0('Q', 1:4))) %>%
  dplyr::select(year, quarter, month, year, season, season_label, precip_seasons, water_year, Treatment, date, VWC, VWC_raw)

df <- df %>% left_join(periods, by = 'year')


# ---------- annual soil moisture -------------------------------------------------#
annual_VWC <- 
  df %>% 
  group_by( Period, year, Treatment ) %>%
  summarise (avg_VWC = mean(VWC_raw, na.rm = TRUE))

# ---------- monthly soil moisture --------------------------------------------------# 

monthly_avgs <- 
  df %>% 
    filter( Treatment == 'Control') %>% 
    group_by( month, year ) %>%
    summarise (avg_VWC = mean(VWC_raw, na.rm = TRUE) )  


png( 'figures/longterm_monthly_soilwat.png', height = 5, width = 5, res = 300, units = 'in')
print( 
ggplot( monthly_avgs, aes( x = factor( month) , y = avg_VWC) ) + 
  geom_boxplot()  + 
  ylim( 0, 35) + 
  ylab( 'Soil water content (ml/ml)') + 
  xlab( 'Month') + 
  my_theme 
)
dev.off()
# ---------- seasonal soil moisture  -----------------------------------------------#
seasonal_VWC <- 
  df %>% 
  mutate(year = ifelse(month == 12 , year + 1, year  )) %>% # account for December 
  group_by(Period, year, season, Treatment) %>% 
  summarise( avg = mean(VWC_raw, na.rm = TRUE) )

# ------------ aggregate VWC with Treatment effects by quarter ---------------#

quarterly_VWC <-
  df %>% 
  group_by( Period, Treatment, year, quarter ) %>%                       
  summarise( avg = mean(VWC_raw, na.rm = TRUE)) 

# -------- output -----------------------------------------------------------------------------#

write.csv( seasonal_VWC, file.path(climate_dir, 'seasonal_VWC.csv'), row.names= F)
write.csv( quarterly_VWC, file.path(climate_dir, 'quarterly_VWC.csv'), row.names= F)
write.csv( annual_VWC, file.path(climate_dir, 'annual_VWC.csv'), row.names= F)

# ------- Some figure output need to move to a separate script ---------------------------------# 

seasonal_VWC$Period[ seasonal_VWC$year > 2010 ] <- 'Modern'
seasonal_VWC$Period[ seasonal_VWC$year < 2011 ] <- 'Historical'

historical_avgs <- 
  seasonal_VWC %>% 
  filter( Period == 'Historical')%>%
  group_by( Period, season ) %>% 
  summarise(lower = quantile(avg, 0.05), upper = quantile(avg, 0.95), avg = mean(avg) ) %>% 
  spread( Period, avg )

modern <- 
  seasonal_VWC %>% 
  filter( Period == 'Modern') %>% 
  group_by( Period, season, year ) 

modern <- merge( modern, historical_avgs )

modern$season <- factor(modern$season, levels = c('winter', 'spring', 'summer', 'fall'), ordered = T)

# make plot of seasonal moisture during the experiment  ------------------------------------------- # 

png('figures/modern_soil_moisture_comparison.png', width = 6, height = 6, res = 300, units  = 'in')

print( 
ggplot( modern %>% filter( year > 2011), aes( x = year, y = avg, color = Treatment ) ) + 
  geom_line() + 
  geom_point() + 
  facet_grid(season ~ . ) + 
  geom_hline( aes( x = year, yintercept  = lower), linetype = 2, alpha = 0.5) + 
  geom_hline( aes( x = year, yintercept = upper ), linetype = 2, alpha = 0.5) + 
  scale_color_manual(values = my_colors[2:4]) + 
  ylab( 'Average seasonal soil moisture content (ml/ml)') + 
  theme_bw() + 
  theme( panel.grid =  element_blank()) 
)

dev.off()

png('figures/modern_soil_moisture_comparison_spring.png', width = 5, height = 3, res = 300, units  = 'in')

print( 
  ggplot( data = subset(modern, modern$season == 'spring' & modern$year > 2011), aes( x = year, y = avg, color = Treatment ) ) + 
    geom_line() + 
    geom_point() + 
    geom_hline( aes( x = year, yintercept  = lower), linetype = 2, alpha = 0.5)  + 
    geom_hline( aes( x = year, yintercept = upper ), linetype = 2, alpha = 0.5)  + 
    scale_color_manual(values = my_colors[2:4])  + 
    ylab( 'Average seasonal soil moisture content (ml/ml)')  + 
    theme_bw() + 
    theme( panel.grid =  element_blank()) 
)

dev.off()

  