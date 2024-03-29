###########################################################################################################
#
# Function to load the environmental layers of the region for which 
# the environmental suitability should be predicted 
# This function is part of the SDM workflow.
# This version is used, when land cover data is provided in the run script.
#
# Author: Larissa Nowak
##########################################################################################################

loadBaseEnvLC2 <- function(envir,filecropbase,landcov){ ## start of main function
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5) 
  # get bioclimatic data
  newenvstack <- raster::subset(fullenvir, envir) 
  # subset climate data to desired climate variables
  rm(fullenvir) # remove from environment to save space
  
  LCStack <- stack(c("LC1.tif","LC2.tif", "LC3.tif", "LC4.tif", "LC5.tif", "LC6.tif", "LC7.tif", "LC8.tif", "LC9.tif", "LC10.tif",
                     "LC11.tif","LC12.tif", "LC13.tif", "LC14.tif", "LC15.tif", "LC16.tif", "LC17.tif", "LC18.tif", "LC19.tif", "LC20.tif",
                     "LC21.tif","LC22.tif", "LC23.tif", "LC24.tif", "LC25.tif", "LC26.tif", "LC27.tif", "LC28.tif", "LC29.tif", "LC30.tif",
                     "LC31.tif","LC32.tif", "LC33.tif", "LC34.tif", "LC35.tif", "LC36.tif", "LC37.tif", "LC38.tif", "LC39.tif", "LC40.tif",
                     "LC41.tif","LC42.tif", "LC43.tif", "LC44.tif")) #load the raster stack with the land cover data
  # note LC38 is somehow missing
  
  LCStack <- subset(LCStack, landcov) # subset the land cover data to the environmental data of interest as specified by the user
  
  newenvstack <- crop(newenvstack, LCStack) # crop climate data to the extend of the land cover data
  
  newenvstack <- stack(newenvstack, LCStack) # stack the climate and the land cover layers
  
  plot(newenvstack) # plot the layers
  
  if(!is.null(filecropbase)) {newenvstack <- crop(newenvstack, filecropbase)} # crop to extend chosen by user (if any)

  newenvirtab <- rasterToPoints(newenvstack) # prepare data frame to be used in the predictions
  newenvirtab <- as.data.frame(newenvirtab)
  newenvirtab <- newenvirtab[complete.cases(newenvirtab),]
  
  return(newenvirtab)

} ## end of main function