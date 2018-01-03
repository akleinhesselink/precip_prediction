#!/usr/bin/env Rscript

rm(list = ls())
library(ggplot2)

# save plotting theme to be used on all plots 

my_theme <- 
  theme_bw () + 
  theme ( panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  strip.background = element_blank())


# black historical 
# olive control-modern 
# orange drought 
# blue  irrigation 


my_colors <- c('black', '#1b9e77', '#d95f02', '#7570b3') 

species_names <- c(bquote( italic('A. tripartita')), bquote(italic('H. comata')), bquote(italic('P. secunda')), bquote(italic('P. spicata')))

pdf_settings <- c('height' = 5, width = 5 ) 

save(my_colors, species_names, my_theme, file = 'figures/my_plotting_theme.Rdata')
