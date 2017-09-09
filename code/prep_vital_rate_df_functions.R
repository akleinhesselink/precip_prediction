# clean dataframe functions 
# functions for preparing vital rate and climate data for analysis 
# scale climate data
# merge growth and survival 
# merge vital rate with climate data

# functions -------------------------------------------------------------------------- # 

set_up_basic_vars <- function(x){ 
  
  require(stringr)
  
  x$Treatment <- factor(x$Treatment)
  x$obs_id <- as.numeric( row.names(x))
  x$yid <- as.numeric( factor(x$year)) 
  x$gid <- as.numeric( factor(x$Group))
  x$quad <- as.numeric(str_extract(x$quad, '[0-9]+$')) 
  x$treat <- as.numeric(factor(x$Treatment))
  x$spp <- as.numeric(factor( x$species))
  
  return(x)
}

clean_growth_survival <- function(clim, growth_file, survival_file){
  gdat <- read.csv( growth_file)
  sdat <- read.csv( survival_file)
  
  # merge growth, survival and climate data -------------------------------------- #
  sdat$logarea.t0 <- sdat$logarea
  x <- merge(sdat, gdat, all.x = T)
  x <- merge(x, clim)
  
  x$Period <- ifelse(x$year > 2011, "Modern", "Historical")
  
  x <- set_up_basic_vars(x)
  
  x$X <- scale(x$logarea.t0) # center X on mean.  Center BEFORE splitting up survival and growth, modern and historical
  
  W <- x[ , grep ( '^W\\.', names( x))]
  W <- as.matrix( W )[,1:4] # big four competition effects
  W <- scale( W ) # scale competition
  x$W <- W
  return(x)
}


clean_recruitment <- function(clim, recruitment_file){ 
  
  require(stringr)
  
  x <- read.csv( recruitment_file)
  x$Period <- ifelse(x$year > 2011, "Modern", "Historical")
  x <- merge(x, clim )

  x <- set_up_basic_vars(x) 
  
  x$gm <- model.matrix.lm(~ x$Group)
  x$tm <- model.matrix.lm(~ x$Treatment)[ , -1]
  
  x$C <- as.matrix( x[ ,  grep( '^C\\.', names(x) ) ] )
  colnames(x$C) <- str_replace( colnames( x$C ) , '^C\\.' , '')
  
  x$parents1 <- as.matrix( x[ , grep( '^cov\\.', names(x))] )
  x$parents2 <- as.matrix( x[ , grep( '^Gcov\\.', names(x))] )
  
  return(x)
}

scale_climate_vars <- function( clim_file, clim_vars){ 
  clim <- read.csv(clim_file)  
  clim <- clim[ ,c('Treatment', 'year', clim_vars)]
  names ( clim ) [ grep( '^(P\\.)|(VWC\\.)|(T\\.)',  names( clim ) ) ] <- paste0 ( 'C.', names( clim ) [ grep( '^(P\\.)|(VWC\\.)|(T\\.)', names(clim ) ) ] )
  clim <- clim[ complete.cases(clim), ] 
  clim[ , grep( 'C\\.', names(clim))] <- scale(clim[ , grep( 'C\\.', names(clim))]) # scale climate data 
  
  return(clim)
}