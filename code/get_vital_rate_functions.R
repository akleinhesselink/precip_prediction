## 
# Helper functions for importing vital rate data from the driversdata directory 
#


set_min_size <- function( size , min_size = 0.25, round_to = 4){ 
  
  size[ round(size, round_to) <= min_size ] <- min_size 
  return(size)  
}


fetch_growth_data <- function(doSpp,speciesList,datadir,distWts){

  growDfile=paste(datadir,"/speciesData/",doSpp,"/growDnoNA.csv",sep="")
  growD=read.csv(file=growDfile)
  D1=growD[growD$allEdge==0,];
  D1$year <- D1$year
  
  D1$area.t0 <- set_min_size(D1$area.t0)
  D1$area.t1 <- set_min_size(D1$area.t1)

  D1$logarea.t0=log(D1$area.t0)
  D1$logarea.t1=log(D1$area.t1)
  D1$quad=as.character(D1$quad)
  
  # import neighbor data
  ringD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings.csv",sep=""))
  tmpD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings_allothers.csv",sep=""))
  ringD<-merge(ringD,tmpD)
  ringD$year<-ringD$year
  
  # merge D with ringD (D contains fewer rows)
  D1<-merge(D1,ringD,by.x=c("quad","year","trackID"),by.y=c("quad","year","genetID"))
  D1=D1[order(D1$X),]
  rm(ringD,growD)
  row.names(D1) <- NULL  
  
  # calculate W's (MAKE SURE NOT TO REORDER D!)
  W <- matrix(NA,NROW(D1),length(speciesList))
  colnames(W) <- paste("W.",speciesList,sep="")

  # do big 4
  for(iSpp in 1:4){
    neighborCols=which(substr(names(D1),1,4)==speciesList[iSpp]) # pull out annulus data for the focal species 
    dist_wts <- distWts[,paste0(speciesList[iSpp])]
    C <- data.matrix(D1[,neighborCols]) #matrix of conspecific areas in the annuli 
    W[,iSpp] <- C%*%dist_wts 
  }
  
  # do allcov and allpts
  for(iSpp in 5:6){
    neighborCols=which(substr(names(D1),1,6)==speciesList[iSpp]) # pull out annulus data for the focal species 
    dist_wts <- distWts[,paste0(speciesList[iSpp])]
    C <- data.matrix(D1[,neighborCols]) #matrix of conspecific areas in the annuli 
    W[,iSpp] <- C%*%dist_wts 
  }
  
  #format
  D1 <- D1[,c("quad","year","trackID","age","distEdgeMin","allEdge","QuadName","Grazing","Group","area.t0","logarea.t0","logarea.t1","species")]
  D1 <- cbind(D1,W)
  
  return(D1)

}


fetch_survival_data <- function(doSpp,speciesList,datadir,distWts){
  
  infile=paste(datadir,"/speciesData/",doSpp,"/survD.csv",sep="")
  survD=read.csv(file=infile)
  D1=survD[survD$allEdge==0,];
  D1$year <- D1$year
  D1$logarea=log(D1$area)
  D1$quad=as.character(D1$quad)
  
  # import neighbor data
  ringD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings.csv",sep=""))
  tmpD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings_allothers.csv",sep=""))
  ringD<-merge(ringD,tmpD)
  ringD$year<-ringD$year
  
  # merge D with ringD (D contains fewer rows)
  D1<-merge(D1,ringD,all.x=T,by.x=c("quad","year","trackID"),by.y=c("quad","year","genetID"))
  D1=D1[order(D1$X),]
  rm(ringD)
  row.names(D1) <- NULL  
  
  # calculate W's (MAKE SURE NOT TO REORDER D!)
  W <- matrix(NA,NROW(D1),length(speciesList))
  colnames(W) <- paste("W.",speciesList,sep="")
  
  # do big 4
  for(iSpp in 1:4){
    neighborCols=which(substr(names(D1),1,4)==speciesList[iSpp]) # pull out annulus data for the focal species 
    dist_wts <- distWts[,paste0(speciesList[iSpp])]
    C <- data.matrix(D1[,neighborCols]) #matrix of conspecific areas in the annuli 
    W[,iSpp] <- C%*%dist_wts 
  }
  
  # do allcov and allpts
  for(iSpp in 5:6){
    neighborCols=which(substr(names(D1),1,6)==speciesList[iSpp]) # pull out annulus data for the focal species 
    dist_wts <- distWts[,paste0(speciesList[iSpp])]
    C <- data.matrix(D1[,neighborCols]) #matrix of conspecific areas in the annuli 
    W[,iSpp] <- C%*%dist_wts 
  }
  
  #format
  D1 <- D1[,c("species","quad","year","trackID","age","logarea","survives","distEdgeMin","allEdge","QuadName","Grazing","Group")]
  D1 <- cbind(D1,W)
  
  return(D1)
  
}


process_surv_grow <- function( dataDir1, dataDir2, doSpp, doVr, sppList){ 
  
  # set up distance weights------------------------------------------------
  #dists <- read.csv('~/driversdata/data/idaho/speciesData/IdahoDistanceWeights.csv')
  dists <- read.csv(file.path(dataDir2, 'speciesData', 'IdahoModDistanceWeights_noExptl.csv'))
  dists$allcov <- rowMeans(dists[,1:4])  # for "other" polygons use average of big 4
  dists$allpts <- dists$POSE  # set forb dist wts = smallest grass (POSE)
  
  # import old data--------------------------------------------------------
  if( doVR == 'growth' )  { 
    D1 <- fetch_growth_data(doSpp=doSpp,speciesList=sppList,datadir=dataDir1,distWts=dists)
  }else if( doVR == 'survival' ) { 
    D1 <- fetch_survival_data(doSpp=doSpp,speciesList=sppList,datadir=dataDir1,distWts=dists)
  }
  
  D1$Treatment <- "Control"
  D1$Period <- "Historical"
  
  # import modern data--------------------------------------------------------
  if( doVR == 'growth' )  { 
    D2 <- fetch_growth_data(doSpp=doSpp,speciesList=sppList,datadir=dataDir2,distWts=dists)
  }else if( doVR == 'survival' ) { 
    D2 <- fetch_survival_data(doSpp=doSpp,speciesList=sppList,datadir=dataDir2,distWts=dists)
  }
  
  D2$Period <- "Modern"
  
  # merge in treatment data
  tmp <- read.csv(file.path(dataDir2,"quad_info.csv"))
  tmp <- tmp[,c("quad","Treatment")]
  D2 <- merge(D2,tmp, all.x=T)
  
  # account for removal in baseline years
  if(doSpp!="ARTR"){
    ii <- which(D2$year>=2011 & D2$Treatment=="No_shrub")
    D2$W.ARTR[ii] <- 0
  }else{
    ii <- which(D2$year>=2011 & D2$Treatment=="No_grass")
    D2$W.HECO[ii] <- 0 ; D2$W.POSE[ii] <- 0 ; D2$W.PSSP[ii] <- 0
  }
  
  # combine old and modern
  allD <- rbind(D1,D2)
  rm(D1,D2,tmp)
  
  # clean up dataset ----------------------------------------------
  allD$year[allD$year<2000] <- allD$year[allD$year<2000] + 1900
  
  if(doSpp=="ARTR"){
    keep <- which(is.element(allD$Treatment,c("Control","No_grass", "Irrigation", "Drought")))
  }else{
    keep <- which(is.element(allD$Treatment,c("Control","No_shrub", "Irrigation", "Drought")))
  }
  allD <- allD[keep,]
  
  allD <- species_specific_cleanup(allD, doSpp, doVr) # remove outliers and questionable records
  
  return(allD)
}

process_recruit <- function( dataDir1, dataDir2, doSpp, doVR, sppList){ 
  require(dplyr)
  require(tidyr)
  require(stringr)
  
  # import old data--------------------------------------------------------
  targetSpp <- sppList[ ! sppList %in% c('allcov', 'allpts') ] # don't do other species
  Nspp=length(targetSpp)
  
  for( i in 1:Nspp){
    infile1=file.path(dataDir1, 'speciesData',targetSpp[i], "recArea.csv")
    tmpD=read.csv(infile1)
    tmpD=tmpD[,c("quad","year","NRquad","totParea","Group")]
    names(tmpD)[3]=paste("R.",targetSpp[i],sep="")
    names(tmpD)[4]=paste("cov.",targetSpp[i],sep="")

    if(i==1){
      D=tmpD
    }else{
      D=merge(D,tmpD,all=T)
    }
  }

  D[is.na(D)]<-0  # replace missing values 
  D$year <- D$year+ 1900
  
  D$Treatment <- "Control"
  D$Period <- 'Historical'

  # import modern data--------------------------------------------------------
  for( i in 1:Nspp){ 
    infile1=file.path(dataDir2, 'speciesData', targetSpp[i], "recArea.csv")
    tmpD=read.csv(infile1)
    tmpD=tmpD[,c("quad","year","NRquad","totParea","Group")]
    names(tmpD)[3]=paste("R.",targetSpp[i],sep="")
    names(tmpD)[4]=paste("cov.",targetSpp[i],sep="")
    
    if(i==1){
      D2=tmpD
    }else{
      D2=merge(D2,tmpD,all=T)
    }
  }
  
  D2[is.na(D2)] <- 0  # replace missing values 
  D2$Period <- 'Modern'

  # merge in treatment data
  tmp <- read.csv(file.path(dataDir2, 'speciesData', "quad_info.csv"))
  tmp <- tmp[,c("quad","Treatment")]
  D2 <- merge(D2,tmp, all.x=T)
  
  # combine old and new data
  D=rbind(D,D2)
  rm(D2)
  
  # get rid of removal treatments
  D <- subset(D,Treatment!="No_grass" & Treatment!="No_shrub")
  
  # calculate mean cover by group and year
  tmpD=subset(D,Treatment=="Control") # only use control plots
  tmpD = D[,c("quad","year","Group",paste("cov.",targetSpp,sep=""))]
  
  tmpD=aggregate(tmpD[,4:NCOL(tmpD)],by=list("year"=tmpD$year,
                                             "Group"=tmpD$Group),FUN=mean)
  
  names(tmpD)[3:NCOL(tmpD)]=paste("Gcov.",targetSpp,sep="")

  D=merge(D,tmpD,all.x=T)

  # assign indicator variables -------------------------------------------------------------------------------- # 
  # D$Treatment2 <- D$Treatment
  # D$Treatment2[D$year>2000] <- "Modern"
  # D$Treatment3 <- D$Treatment
  # D$Treatment3[D$Treatment=="Control" & D$year>2000] <- "ControlModern"
  
  # assign group level zeros to smallest non-zero value 
  historical_Gcov <- D[ D$Period == 'Historical', grep ( '^Gcov\\.', names(D) ) ]
  min_cover <- min(historical_Gcov[ historical_Gcov > 0 ] )
  D[ ,grep ( '^Gcov\\.', names(D) )][D[, grep ( '^Gcov\\.', names(D) )] == 0 ] <- min_cover
  
  D <- D %>% 
    gather(species, Y, starts_with('R.')) %>% 
    mutate( species = str_replace(species, pattern = '^R.', replacement = ''))
  
  D <- subset(D, species == doSpp)
  return(D)
}

species_specific_cleanup <- function(dat, doSpp, doVR ) { 
  tmp = NULL   
  if( doVR %in% c('growth', 'survival') ){
    if(doSpp == 'ARTR'){ 
      tmp=which(dat$quad=="Q23" & dat$year==1945 & dat$trackID==67)
      tmp=c(tmp,which(dat$quad=="Q12" & dat$year==1955 & dat$trackID==25))
      tmp=c(tmp,which(dat$quad=="Q26" & dat$year==1945 & dat$trackID==73))
      tmp=c(tmp, which(dat$Grazing == 'G' & dat$year == 1931))                   # remove grazed plots in 1931 big decrease in size 
    }else if(doSpp == 'HECO'){
    }else if(doSpp == 'POSE'){
      tmp <- which(dat$quad == 'Q11' & dat$trackID == '26' & dat$year %in% c(1931,1932, 1938, 1939) ) # drop these observations 
    }else if(doSpp == 'PSSP'){
    }
  }else if( doVR == 'recruitment'){
  } 
  
  if(!is.null(tmp)){ 
    dat <- dat[-tmp, ]
  }
  return(dat)
}

