###########################################################################################################
#
# Function to automatically download WorldClim environmental variables, 
# test for their correlation and add them to the table with the occurrence records of the focal species
# This function is part of the SDM workflow. 
#
# Author: Larissa Nowak
##########################################################################################################

addenvir2 <- function(envir) { ## start of main function
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5)
  # download the full set of bioclimatic variables from worldclim at 2.5 min resolution, result is a raster stack
  envstack <- subset(fullenvir, envir) # subset the raster stack to climate variables of choice
  rm(fullenvir)
    
  plot(envstack)
  corr <- layerStats(envstack, 'pearson', na.rm=T) # correlation test for the climate variables of choice
  
  print("Correlation of environmental predictors:")
  print(corr$'pearson correlation coefficient')
  print("Note: If |cor| > 0.5 consider removing some of the environmental predictors to reduce multicolinearity.")
  # notifications for the user
  
  # - 2) extract environment values from locations at which species occur and merge that with species occurrences in table 
  
  if(length(envir)==1) {occenv <- cbind(occ, envir = raster::extract(x = envstack, y = data.frame(occ[,c('decimalLongitude','decimalLatitude')])))
  colnames(occenv) <- c("decimalLongitude", "decimalLatitude", "Presence", envir[1])} else{occenv <- cbind(occ, raster::extract(x = envstack, y = data.frame(occ[,c('decimalLongitude','decimalLatitude')])))} 
  # combining occurrences with environmental info; the if else clause is needed to avoid weird column names, if only one climate variable is used.
  
  return(occenv)
} ## end of main function