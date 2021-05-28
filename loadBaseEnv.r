###########################################################################################################
#
# Function to load the environmental layers of the region for which 
# the environmental suitability should be predicted 
# This function is part of the SDM workflow.
#
# Author: Larissa Nowak
##########################################################################################################

# Note: so far, this function uses the same environmental data used for model fitting (WorldClim). Here we need to think about a way of combining this with CASPIAN

loadBaseEnv <- function(envir,filecropbase){ ## start of main function
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5) 
  # get environmental data
  
  newenvstack <- raster::subset(fullenvir, envir) 
  # subset environmental data to desired environmental variables
  
  if(!is.null(filecropbase)) {newenvstack <- crop(newenvstack, filecropbase)}

  newenvirtab <- rasterToPoints(newenvstack) # prepare data frame to be used in the predictions
  newenvirtab <- as.data.frame(newenvirtab) 

  return(newenvirtab)

} ## end of main function