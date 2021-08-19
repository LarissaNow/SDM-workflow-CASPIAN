###########################################################################################################
#
# Function to average over the predictions from different model runs
# This function needs to be used, when the GAM prediction was done with parallelization 
# and the predictions are given as a list-object
# This function is part of the SDM workflow.
# This version is used, when land cover data is provided in the run script.
#
# Author: Larissa Nowak
##########################################################################################################

modelaverageParallelLC <- function(suitability, envir, landcov) { ## start of main function
  
  dataempty <- suitability[[1]][,c(1:(length(envir)+length(landcov)+2))] 
  # here I create a data frame with latitude, longitude and bioclim variables
  it <- 1 # this is simply an iterator to give each loop run a separate number in the column name
  
  for(i in suitability) { # in this loop, I collect all the predicted suitabilities over the different model runs and add them to the data frame from above
    xxx <- i[,-c(1:(length(envir)+length(landcov)+2))]
    names(xxx) <- rep(paste0("predictionPA",it),length(names(xxx)))
    dataempty <- cbind(dataempty,xxx)
    it <- it+1
  }
  
  dataempty$modelaverage <- apply(dataempty[,-c(1:(length(envir)+3))], MARGIN = 1, FUN = mean) 
  # compute the mean over the different model runs
  dataempty$modelsd <- apply(dataempty[,-c(1:(length(envir)+3))], MARGIN = 1, FUN = sd) 
  # compute the sd over the different model runs
  
  write.csv(dataempty[,c(1:(length(envir)+3),length(colnames(dataempty))-1, length(colnames(dataempty)))], 
            file.path("output", paste0("averageSuitability_",identifier,".csv"))) 
  # store the predictions as csv-file on the users computer
  
  print("Note: The file 'averageSuitability' has been stored on your computer as csv-file. This file contains the environmental suitability for the focal species in the focal region averaged over the different model runs.") # notification for the user

  return(dataempty)
  } ## end of main function