###########################################################################################################
#
# Function to randomly sample five sets of pseudo absences (PA), extract environmental and land cover data 
# for them and combine them with the occurrence data of the foccal species
# With parallelization
# This function is part of the SDM workflow.
# This version is used, when land cover data is provided in the run script.
#
# Author: Larissa Nowak
##########################################################################################################

# !!NOTE: the type of parallel processing that is used here might only work on Windows. If it is supposed to work for other systems, I probably need to write separate functions and build in some type of automated system detection in the run-script!!

PAsampleParallelLC <- function(occenv, envir, landcov) { ## start of main function
  
  print("Note: The sampling of the pseudoabsences might take several minutes.")
  
  # this is a PA-sampling version without weighing of the potential absence cells.
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5) 
  # prepare environmental data to be combined with pseudoabsences
  envstack <- subset(fullenvir, envir) # this is required to match the PAs with the environment data later.
  rm(fullenvir)
  
  LCStack <- stack(c("LC1.tif","LC2.tif", "LC3.tif", "LC4.tif", "LC5.tif", "LC6.tif", "LC7.tif", "LC8.tif", "LC9.tif", "LC10.tif",
                     "LC11.tif","LC12.tif", "LC13.tif", "LC14.tif", "LC15.tif", "LC16.tif", "LC17.tif", "LC18.tif", "LC19.tif", "LC20.tif",
                     "LC21.tif","LC22.tif", "LC23.tif", "LC24.tif", "LC25.tif", "LC26.tif", "LC27.tif", "LC28.tif", "LC29.tif", "LC30.tif",
                     "LC31.tif","LC32.tif", "LC33.tif", "LC34.tif", "LC35.tif", "LC36.tif", "LC37.tif", "LC39.tif", "LC40.tif",
                     "LC41.tif","LC42.tif", "LC43.tif", "LC44.tif")) #load the raster stack with the land cover data
  # note LC38 is somehow missing
  
  LCStack <- subset(LCStack, landcov) #subset the land cover data to the environmental data of interest as specified by the user
  
  envstack <- crop(envstack, LCStack) # crop the climate data to the extent of the land cover data
  
  mask <- subset(envstack, envir[1]) # take one of the environmental files to have a template with all terrestrial cells to sample from
  # it is important that this has the same extent than the land cover data, to make sure the PAs are sampled from cells for which we have data
  
  n <- 1:5 # to sample 5 alternative PA sets
  
  myfun <-  function(z){ # define function to be used in parallel processing
    PA <- as.data.frame(dismo::randomPoints(mask=mask, p = occenv[,c(1,2)],  n = 10000)) 
    # sample random PAs
    # adjust n to the desired number of PAs!!
    # the argument p gives the presence points from which sampling should be done, n is the number of PAs that should be sampled. If in addition the argument prob is set true, the values of the mask are taken as probabilities (interesting, if a weighing of the potential absence cells is desired).
   
    PA$decimalLongitude <- PA$x # some data transformation to match the occurrence data frame
    PA$decimalLatitude <- PA$y
    PA$x <- NULL
    PA$y <- NULL
    PA$Presence <- 0
   
    if(length(envir)==1) {PAenv <- cbind(PA, envir = raster::extract(x = envstack, y = data.frame(PA[,c('decimalLongitude','decimalLatitude')]))) }
    else{PAenv <- cbind(PA, raster::extract(x = envstack, y = data.frame(PA[,c('decimalLongitude','decimalLatitude')])))} 
    # combining PAs with environmental info; the if else clause is needed to avoid weird column names, if only one climate variable is used.
    
    if(length(landcov)==1) {PAenv <- cbind(PAenv, landcov = raster::extract(x = LCStack, y = data.frame(PA[,c('decimalLongitude','decimalLatitude')])))}
    else{PAenv <- cbind(PAenv, raster::extract(x = LCStack, y = data.frame(PA[,c('decimalLongitude','decimalLatitude')])))}
    # combining PAs with land cover info; the if else clause is needed to avoid weird column names, if only one land cover variable is used.
    
    OccenvPA <- rbind(occenv, PAenv) # combining PAs with occurrences
    OccenvPA <- list(OccenvPA) # transform to a list
    names(OccenvPA) <- paste0("PA",z) # name the PA sample distinctly
   
    return(OccenvPA)
  }
  
  # parallel processing: 
  no_cores <- parallel::detectCores(logical = TRUE) # get number of logical cores of the user's machine
  cl <- parallel::makeCluster(no_cores-1)  # provide clusters to R
  doParallel::registerDoParallel(cl) # register to these clusters
  parallel::clusterExport(cl,list('myfun','n', 'occenv', 'envstack', 'mask', 'landcov', 'LCStack'), envir=environment()) 
  # export all objects and functions to the clusters #envir() indicates that these functions and objects can be found within the environment of the main function
  PAlist <- c(parallel::parLapply(cl,n,fun=myfun)) # apply function in parallel
  stopCluster(cl)
  
  return(PAlist)
  
} ## end of main function
