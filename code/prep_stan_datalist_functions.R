# Functions to format stan data lists 

split_and_format <- function( df , Xcenter, Xscale, Wcenter , Wscale ) { 
  mylist <- split(df, df$Period)
  mylist <- lapply( mylist, as.list )
  mylist$all <- as.list( df ) # save entire data list as 'all' use for predictions 
  
  mylist <- lapply( mylist, function( y ) { y$nyrs = nlevels(factor(y$yid)); y } )
  mylist <- lapply( mylist, function( y ) { y$N = length(y$Y); y})
  
  names(mylist$Modern) <- paste( names(mylist$Modern) ,'hold', sep = '' ) 
  names(mylist$all) <- paste( names(mylist$all), 2, sep = '')
  mylist <- unlist(mylist, recursive = F, use.names = T)
  names( mylist ) <- str_replace( names(mylist) , '^.*\\.', '') # clean up names 
  
  mylist$nT    <- ncol(mylist$tm)
  mylist$G     <- ncol(mylist$gm)
  mylist$Wcovs <- ncol ( mylist$W )
  mylist$Covs <- ncol ( mylist$C ) 
  mylist$spp <- unique(df$spp)
  mylist$tau_beta <- 10 
  mylist$Xcenter <- Xcenter
  mylist$Xscale  <- Xscale
  mylist$Wcenter <- Wcenter
  mylist$Wscale  <- Wscale 
  return(mylist)
}


make_growth_and_survival_datlist <- function(x){ 
  
  Xcenter <- attr( x$X, 'scaled:center') # save mean       
  Xscale  <- attr( x$X, 'scaled:scale' ) # save sd 
  x$X <- as.numeric(x$X)
  
  Wcenter <- attr( x$W, 'scaled:center')  # save scaled mean
  Wscale  <- attr( x$W, 'scaled:scale' )  # save scaled scale
  
  x$gm <- model.matrix.lm( ~ x$Group ) 
  
  x$tm <- model.matrix.lm( ~ x$Treatment )[, -1 ]  # drop intercept 
  x$tm <- cbind ( x$tm , x$tm*x$X )  # competition by climate effect 
  colnames(x$tm)
  colnames(x$tm) <- c('Drought' , 'Irrigation', 'Droughtxlogarea.t0', 'Irrigationxlogarea.t0')
  
  x$C <- as.matrix( x[ ,  grep( '^C\\.', names(x) ) ] )
  colnames(x$C) <- str_replace( colnames( x$C ) , '^C\\.' , '')
  
  x <- 
    x %>% 
    dplyr::select( X, year, treat, Period, spp, quad, trackID, W, obs_id, C, yid, gid, gm, tm, logarea.t1, survives ) %>% 
    rename( trackid = trackID)
  
  # split into lists for growth and survival 
  names(x) <- str_replace (  names( x), '\\.', '_' )
  
  growth <- x[ !is.na(x$logarea_t1), ]
  survival <- x[ !is.na(x$survives), ]
  
  survival$Y <- survival$survives
  growth$Y <- growth$logarea_t1
  
  survival <- survival[, -which(names(survival) == 'logarea_t1')] # drop logarea_t1
  
  survival <- survival[ complete.cases( survival ), ] 
  growth <- growth[ complete.cases( growth ), ] 
  
  sdl <- split_and_format(survival, Xcenter, Xscale,  Wcenter, Wscale )  
  gdl <- split_and_format(growth, Xcenter , Xscale, Wcenter, Wscale )
  
  return( list(sdl, gdl) ) 
} 

make_recruitment_datlist <- function(x){ 
  x <- 
    x %>% 
    dplyr::select( Y, year, treat, Period, spp, quad, obs_id, C, yid, gid, parents1, parents2, gm, tm ) 
  
  mylist <- split( x , x$Period)
  
  mylist <- lapply( mylist, as.list )
  
  mylist$all <- as.list( x )
  
  mylist <- lapply( mylist, function( y ) { y$nyrs = nlevels(factor(y$yid)); y } )
  mylist <- lapply( mylist, function( y ) { y$nT = ncol(y$tm); y } )
  mylist <- lapply( mylist, function( y ) { y$G = nlevels(factor(y$gid)); y } )
  mylist <- lapply( mylist, function( y ) { y$N = length(y$Y); y})
  
  names(mylist$Modern) <- paste( names(mylist$Modern) ,'hold', sep = '' ) 
  names(mylist$all) <- paste( names(mylist$all), 2, sep = '')
  
  mylist <- unlist(mylist, recursive = F, use.names = T)
  
  names( mylist ) <- str_replace( names(mylist) , '^.*\\.', '') # clean up names 
  
  mylist$Period <- as.numeric( factor( mylist$Period ) )
  mylist$Periodhold <- as.numeric( factor(mylist$Periodhold))
  mylist$Period2 <- as.numeric( factor(mylist$Period2))
  
  mylist$Nspp <- ncol ( mylist$parents1 )
  mylist$Covs <- ncol ( mylist$C ) 
  mylist$spp <- unique( x$spp ) 
  mylist$tau_beta <- 10 
  
  return( mylist ) 
}

add_survival_data <- function( growth , survival ) { 
  
  cover_list <- survival [ grep ( '*.2$', names( survival ) ) ] 
  names(cover_list )  <- str_replace(names(cover_list) , '2$', '3')
  out <- c( growth, cover_list)
  
  return( out ) 
  
} 