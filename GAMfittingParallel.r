GAMfitting <- function(PAlist) {
  for(i in PAlist) {
    
    data.model <- cbind(as.data.frame(rownames(i[[1]])),i[[1]])
    
    preds <- colnames(data.model[5:length(colnames(data.model))]) # 1 is the index, 2 and 3 are long, lat and 4 is the presence
    
    formula <- as.formula(paste("Presence ~ ",paste(sapply(preds,function(x,...) paste0("s(",x,", k=20",")")),collapse="+",sep=" ")))
    # s() makes sure, a predictor is fitted via several basic functions, k defines the number of basic functions that is fitted
    # here I need to create the formula for the GAM
    family <- "binomial" #because it is presence absence data (0/1)
    
    r <- 1:2 # Repeat modelling ten times on a 30/70 split #adjust number according to the number of runs you want to have
    
    final.mods <- lapply(r,function(x){ # this creates a list with the model outputs plus AUC of the ten model runs (fitted with ten   different random 70-30 data splits)
      smp_size <- floor(0.3 * nrow(data.model)) # 70-30 split is done here
      train_ind <- sample(seq_len(nrow(data.model)), size = smp_size)
      fit.blocks <- data.model[-train_ind, ]
      test.block <- data.model[train_ind, ]
      
      possibleError <- tryCatch(model1 <- gam(formula, family=family, data=fit.blocks, method = "REML"), 
                                # model is fitted here
                                # method gives the method that is used to automatically find the right smooting parameter
                                # gamma Increase this beyond 1 to produce smoother models; gamma=1.4
                                # here I could also insert a vector for sp, that gives the smoothing parameter to be used for each parameter
                                error=function(e) e)
      
      if(!inherits(possibleError, "error")){
        
        blockNo <- x
        
        ## Predict the fitted model to the predictions block
        PRED <- as.data.frame(predict(model1,newdata=test.block,type="response",se.fit=FALSE)) #predictions are done here
        colnames(PRED) <- paste("pred",blockNo,sep=".")
        eval.data.auc <- cbind(test.block[,c(1,4)],PRED)
        
        ## Threshold independent - Set AUC
        AUC <- round(auc(eval.data.auc,st.dev=FALSE,na.rm=T),4)
        return(list(Block=blockNo,AUC=AUC,mod=model1)) }
      
    })
    
    assign(paste0(names(i[1]),"final.mods"),final.mods)}
  # how to put them together to one list?
  
  modelruns100 <- list(PA1final.mods, PA2final.mods, PA3final.mods, PA4final.mods, PA5final.mods, PA6final.mods, PA7final.mods, PA8final.mods, PA9final.mods, PA10final.mods)
  
  #save(modelruns100, file="modelruns100.RData")
  return(modelruns100)
} ##Enf of function ##