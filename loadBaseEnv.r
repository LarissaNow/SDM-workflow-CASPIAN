###########################################################################################################
#
# Function to load the environmental layers of the region for which 
# the environmental suitability should be predicted 
# This function is part of the SDM workflow.
#
# Author: Larissa Nowak
##########################################################################################################

loadBaseEnv <- function(envir,filecropbase){ ## start of main function
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5) 
  # get bioclimatic data
  
  newenvstack <- raster::subset(fullenvir, envir) 
  # subset climate data to desired environmental variables
  
  if(!is.null(filecropbase)) {newenvstack <- crop(newenvstack, filecropbase)} # crop to extend chosen by user (if any)

  newenvirtab <- rasterToPoints(newenvstack) # prepare data frame to be used in the predictions
  newenvirtab <- as.data.frame(newenvirtab) 

  return(newenvirtab)

} ## end of main function