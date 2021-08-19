###########################################################################################################
#
# Function to predict environmental suitability for the different model runs with parallelization
# This function is part of the SDM workflow.
#
# Author: Larissa Nowak
##########################################################################################################

# !!NOTE: the type of parallel processing that is used here might only work on Windows. If it is supposed to work for other systems, I probably need to write separate functions and build in some type of automated system detection in the run-script!!

predictGAMParallel <- function(baseenvir, modelruns100) { ## start of main function
  
  print("Note: Predicting environmental suitabilities might take several minutes. If predicted over the entire world it can take up to 30 minutes or more depending on your machine.")
  
  prednewenvir <- baseenvir # prepare data frame to store predictions in
  
  funtem <- function(listele){ ## start of sub function
    # the function will be applied over the list of PA samples, the loop loops through the five data-splits per PA sample
    
    for(n in listele){if(n$AUC>0.7|!is.na(n$AUC)){ # remove models with a bad AUC
   
    testpred <- as.data.frame(predict(n$mod,newdata=baseenvir,type="response",se.fit=FALSE)) 
    # predict suitabilities (this part of the function is slowest)
    
    colnames(testpred) <- paste0("testpred", n$Block)
    
    prednewenvir <- cbind(prednewenvir,testpred) 
    # add predictions to environmental data frame
    } ## end of ifclause
      } ## end of loop
      return(prednewenvir)} ## end of sub function
  
  #parallel processing: 
  no_cores <- parallel::detectCores(logical = TRUE) # get number of logical cores of the user's machine
  cl <- parallel::makeCluster(no_cores-1)  # provide clusters to R
  doParallel::registerDoParallel(cl) # register to these clusters
  parallel::clusterCall(cl, function() library(mgcv)) 
  # make the mgcv package accesible in each cluster, this is needed to allow predictions from a GAM
  parallel::clusterExport(cl,list('funtem','modelruns100', 'prednewenvir', 'baseenvir'),envir=environment())
  predictionslist <- c(parallel::parLapply(cl,modelruns100,fun=funtem)) # this runs the function from above in parallel on the different PA samples in the list modelruns100 and saves the ouput as list
  stopCluster(cl)
  
  return(predictionslist)
  
} ## end of main function
