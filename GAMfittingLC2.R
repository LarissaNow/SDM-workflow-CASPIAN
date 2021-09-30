###########################################################################################################
#
# Function to fit five generalized additive models (GAM) over five different random 70-30 data splits
# and computes the area under the curve (AUC) for each model
# Parts of this function are adapted from https://github.com/christianhof/BioScen1.5_SDM/blob/master/R
# This function is part of the SDM workflow.
# This version is used, when land cover data is provided in the run script.
#
# Author: Larissa Nowak
##########################################################################################################


GAMfittingLC2 <- function(PAlist) { ## start of main function
  
  print("Note: The fitting of the models might take several minutes.")
  
  for(i in PAlist) {
    
    data.model <- cbind(as.data.frame(rownames(i[[1]])),i[[1]])
    data.model <- data.model[complete.cases(data.model),]
    
    preds <- colnames(data.model[5:length(colnames(data.model))]) # select the names of the predictors; 1 is the index, 2 and 3 are long, lat and 4 is the presence
    
    formula <- as.formula(paste("Presence ~ ",paste(sapply(preds,function(x,...) paste0("s(",x,", k=20",")")),collapse="+",sep=" ")))
    # this creates the formula for the GAMs
    # s() makes sure, a predictor is fitted via several basic functions, 
    # k defines the number of basic functions that is fitted
    
    family <- "binomial" # because the dependent variable is presence absence data (0/1)
    
    r <- 1:5 # Repeat modelling five times on a 30/70 split #adjust number according to the number of runs you want to have
    
    final.mods <- lapply(r,function(x){ # this creates a list with the model outputs plus AUC of the five model runs (fitted with five different random 70-30 data splits)
      smp_size <- floor(0.3 * nrow(data.model)) # 70-30 split is done here
      train_ind <- sample(seq_len(nrow(data.model)), size = smp_size)
      fit.blocks <- data.model[-train_ind, ]
      test.block <- data.model[train_ind, ]
      
      possibleError <- tryCatch(model1 <- gam(formula, family=family, data=fit.blocks, method = "REML"), 
                                # model is fitted here
                                # method gives the method that is used to automatically find the right smooting parameter, in this case "restcricted maximum likelihood"
                                # gamma: Increase this beyond 1 to produce smoother models; gamma=1.4
                                # here I could also insert a vector for sp, that gives the smoothing parameter to be used for each parameter
                                error=function(e) e)
      
      if(!inherits(possibleError, "error")){
        
        blockNo <- x
        
        PRED <- as.data.frame(predict(model1,newdata=test.block,type="response",se.fit=FALSE)) 
        # predictions are done here
        colnames(PRED) <- paste("pred",blockNo,sep=".")
        eval.data.auc <- cbind(test.block[,c(1,4)],PRED)
        
        AUC <- round(auc(eval.data.auc,st.dev=FALSE,na.rm=T),4) # compute AUC
        return(list(Block=blockNo,AUC=AUC,mod=model1)) }
      
    })
    
    assign(paste0(names(i[1]),"final.mods"),final.mods)} # this creates an object with a distinct name and stores the model-runs for one PA sample in it
  
  modelruns100 <- list(PA1final.mods, PA2final.mods, PA3final.mods, PA4final.mods, PA5final.mods)
  # combine the different model runs into one list
  # this is a bit of a 'unelegant' solution, as the names of the objects need to be changed, everytime the number of PA samples is changed in the previous step
  
  return(modelruns100)
  
} ## end of main function