###########################################################################################################
#
# Function to automatically download WorldClim environmental variables, 
# test for their correlation and add them to the table with the occurrence records of the focal species.
# In addition, it tests for an association of the land cover data and the climate data
# This function is part of the SDM workflow. 
# This version is used, when land cover data is provided in the run script.
#
# Author: Larissa Nowak
##########################################################################################################


addenvirLC2 <- function(envir, landcov) { ## start main function
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5) #this is from the package raster
  envstack <- subset(fullenvir, envir) #subset the worldclim data to the environmental data of interest as specified by the user
  rm(fullenvir)
  
  LCStack <- stack(c("LC1.tif","LC2.tif", "LC3.tif", "LC4.tif", "LC5.tif", "LC6.tif", "LC7.tif", "LC8.tif", "LC9.tif", "LC10.tif",
                   "LC11.tif","LC12.tif", "LC13.tif", "LC14.tif", "LC15.tif", "LC16.tif", "LC17.tif", "LC18.tif", "LC19.tif", "LC20.tif",
                   "LC21.tif","LC22.tif", "LC23.tif", "LC24.tif", "LC25.tif", "LC26.tif", "LC27.tif", "LC28.tif", "LC29.tif", "LC30.tif",
                   "LC31.tif","LC32.tif", "LC33.tif", "LC34.tif", "LC35.tif", "LC36.tif", "LC37.tif", "LC39.tif", "LC40.tif",
                   "LC41.tif","LC42.tif", "LC43.tif", "LC44.tif")) #load the raster stack with the land cover data
  # note LC38 is somehow missing
  
  LCStack <- subset(LCStack, landcov) #subset the land cover data to the environmental data of interest as specified by the user
 
  envstack <- crop(envstack, LCStack) # crop the climate data to the extent of the land cover data
  
  #envstack2 <- stack(envstack, LCStack) # okay, I cannot stack them because they have a different extent, I need to fix that! 
  
  corr <- layerStats(envstack, 'pearson', na.rm=T) # not yet adapted for habitat data! For this it needs to be a different type of correlation (spearman)
  
  # NOTE: once I have the land cover data prepared by Hanno, they should be the same resolution and extent than the world clim data, so that I can combine them to one raster stack and do the correlation test for all of them!
  
  print("Correlation of environmental predictors:")
  print(corr$'pearson correlation coefficient')
  print("Note: If |cor| > 0.7 consider removing some of the environmental predictors to reduce multicolinearity.")
  # notifications for the user
  
  # - 2) extract environment values from locations at which species occur and merge that with species occurrences in table 
  if(length(envir)==1) {occenv <- cbind(occ, envir = raster::extract(x = envstack, y = data.frame(occ[,c('decimalLongitude','decimalLatitude')])))}
  else{occenv <- cbind(occ, raster::extract(x = envstack, y = data.frame(occ[,c('decimalLongitude','decimalLatitude')])))} 
  # extract environmental info for grid cells in which the target species occurs
  
  if(length(landcov)==1) {occenv <- cbind(occenv, landcov = raster::extract(x = LCStack, y = data.frame(occ[,c('decimalLongitude','decimalLatitude')])))}
  else{occenv <- cbind(occenv, raster::extract(x = LCStack, y = data.frame(occ[,c('decimalLongitude','decimalLatitude')])))}
  # extract land cover data for the grid cell in which the species occurs
  
  occenv <- occenv[complete.cases(occenv),] #remove occurrence points without environmental data
  
  print("Note: The following land cover variables only take on the value 0 accross the occurrences of your focal species. Please remove/replace these variables to avoid errors while  model fitting.") # notification for the user
  print(names(which(colSums(occenv)==0)))
  
  return(occenv) 
} ## end of main function
