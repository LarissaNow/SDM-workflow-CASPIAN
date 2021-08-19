###########################################################################################################
#
# Function to load the environmental layers of the region for which 
# the environmental suitability should be predicted 
# This function is part of the SDM workflow.
# This version is used, when land cover data is provided in the run script.
#
# Author: Larissa Nowak
##########################################################################################################

# Note: so far, this function uses the same environmental data used for model fitting (WorldClim). Here we need to think about a way of combining this with CASPIAN

loadBaseEnvLC2 <- function(envir,filecropbase,landcov){ ## start of main function
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5) 
  # get environmental 
  newenvstack <- raster::subset(fullenvir, envir) 
  # subset environmental data to desired environmental variables
  rm(fullenvir) # remove from environment to save space
  
  LCStack <- stack(c("LC1.tif","LC2.tif", "LC3.tif", "LC4.tif", "LC5.tif", "LC6.tif", "LC7.tif", "LC8.tif", "LC9.tif", "LC10.tif",
                     "LC11.tif","LC12.tif", "LC13.tif", "LC14.tif", "LC15.tif", "LC16.tif", "LC17.tif", "LC18.tif", "LC19.tif", "LC20.tif",
                     "LC21.tif","LC22.tif", "LC23.tif", "LC24.tif", "LC25.tif", "LC26.tif", "LC27.tif", "LC28.tif", "LC29.tif", "LC30.tif",
                     "LC31.tif","LC32.tif", "LC33.tif", "LC34.tif", "LC35.tif", "LC36.tif", "LC37.tif", "LC39.tif", "LC40.tif",
                     "LC41.tif","LC42.tif", "LC43.tif", "LC44.tif")) #load the raster stack with the land cover data
  # note LC38 is somehow missing
  
  LCStack <- subset(LCStack, landcov) #subset the land cover data to the environmental data of interest as specified by the user
  
  if(!is.null(filecropbase)) {newenvstack <- crop(newenvstack, filecropbase)}

  newenvirtab <- rasterToPoints(newenvstack) # prepare data frame to be used in the predictions
  newenvirtab <- as.data.frame(newenvirtab)
  
  if(length(landcov)==1){newenvirtab2 <- cbind(newenvirtab, landcov = raster::extract(x = LCStack, y = data.frame(newenvirtab[,c('x','y')])))}
  else{newenvirtab2 <- cbind(newenvirtab, raster::extract(x = LCStack, y = data.frame(newenvirtab[,c('x','y')])))} # add land cover data
  #newenvirtab <- newenvirtab[complete.cases(newenvirtab),] # subset to grid cells with land cover data, so far only available for Europe

  newenvirtab2 <- newenvirtab2[complete.cases(newenvirtab2),] #to subset the data to cells for which land cover data is available
  
  return(newenvirtab2)

} ## end of main function