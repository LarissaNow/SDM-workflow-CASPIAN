###########################################################################################################
#
# Function to automatically download WorldClim environmental variables, 
# test for their correlation and add them to the table with the occurrence records of the focal species
# This function is part of the SDM workflow. 
#
# Author: Larissa Nowak
##########################################################################################################

addenvir2 <- function(envir) { ## start main function
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5) #this is from the package raster
  
  envstack <- subset(fullenvir, envir)
  
  plot(envstack) #note: habitat data is still missing
  corr <- layerStats(envstack, 'pearson', na.rm=T) # not yet adapted for habitat data! For this it needs to be a different type of correlation
  
  print("Correlation of environmental predictors:")
  print(corr$'pearson correlation coefficient')
  print("NOTE: If cor > 0.7 consider removing some of the environmental predictors to reduce multicolinearity.")
  # notifications for the user
  
  # - 2) extract environment values from locations at which species occur and merge that with species occurrences in table 
  
  occenv <- cbind(occ, raster::extract(x = envstack, y = data.frame(occ[,c('decimalLongitude','decimalLatitude')]))) 
  # extract environmental info for grid cells in which the target species occurs
  
  return(occenv)
} ## end of main function