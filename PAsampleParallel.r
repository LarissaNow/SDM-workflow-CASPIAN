###########################################################################################################
#
# Function to randomly sample five sets of pseudo absences (PA), extract environmental data for them 
# and combine them with the occurrence data of the foccal species
# With parallelization
# This function is part of the SDM workflow. 
#
# Author: Larissa Nowak
##########################################################################################################

# !!NOTE: the type of parallel processing that is used here might only work on Windows. If it is supposed to work for other systems, I probably need to write separate functions and build in some type of automated system detection in the run-script!!

PAsampleParallel <- function(occenv, envir) { ## start of main function
  
  print("Note: The sampling of the pseudoabsences might take several minutes.")
  
  # this is a PA-sampling version without weighing of the potential absence cells.
  
  fullenvir <- raster::getData(name = "worldclim",var = "bio", res = 2.5) 
  # prepare environmental data to be combined with pseudoabsences
  
  envstack <- subset(fullenvir, envir) # this is required to match the PAs with the environment data later.
  
  mask <- subset(fullenvir, envir[1]) # take one of the environmental files to have a template with all terrestrial cells to sample from
  
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
   
    PAenv <- cbind(PA, raster::extract(x = envstack, y = data.frame(PA[,c('decimalLongitude','decimalLatitude')]))) 
    # combining PAs with environmental info
   
    OccenvPA <- rbind(occenv, PAenv) # combining PAs with occurrences
    OccenvPA <- list(OccenvPA) # not sure anymore, why I did this...
    names(OccenvPA) <- paste0("PA",z) # name the PA sample distinctly
   
    return(OccenvPA)
  }
  
  # parallel processing: 
  no_cores <- parallel::detectCores(logical = TRUE) # get number of logical cores of the user's machine
  cl <- parallel::makeCluster(no_cores-1)  # provide clusters to R
  doParallel::registerDoParallel(cl) # register to these clusters
  parallel::clusterExport(cl,list('myfun','n', 'occenv', 'envstack', 'mask'),envir=environment()) 
  # export all objects and functions to the clusters
  PAlist <- c(parallel::parLapply(cl,n,fun=myfun)) # apply function in parallel
  stopCluster(cl)
  
  return(PAlist)
  
} ## end of main function