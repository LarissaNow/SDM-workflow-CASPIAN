###########################################################################################################
#
# Function to plot the average predicted environmental suitabilities
# This function is part of the SDM workflow.
#
# Author: Larissa Nowak
##########################################################################################################

plotSuitabilities <- function(avgsuitability, occ) { ## start of main function
  
  avgsuitability2 <- avgsuitability[,c("x", "y", "modelaverage")] # prepare raster file with the mean predictions used for plotting
  coordinates(avgsuitability2) <- ~ x + y
  gridded(avgsuitability2) <- T
  rastpreds <- raster(avgsuitability2)
  
  par(bty="l")
  
  plot(rastpreds, col=viridis(100)) # plot the predicted probabilities
  
  if(!is.null(filecropbase)) {points(occ, pch=1, cex=0.5, xlim=filecropbase[1:2], ylim=filecropbase[3:4], col="white")
    } else{points(occ, pch=1, cex=0.5, col="black")} # plot occurrences
  
  # store plots on the user computer:
  pdf(file.path("output", paste0("Suitability_",identifier,".pdf"))) # plot without occurrences
  plot(rastpreds, col=viridis(100))
  dev.off()
 
   pdf(file.path("output", paste0("SuitabilityOccurrences_",identifier,".pdf"))) # plot with occurrences
  plot(rastpreds, col=viridis(100))
  if(!is.null(filecropbase)) {
    points(occ, pch=1, cex=0.5, xlim=filecropbase[1:2], ylim=filecropbase[3:4], col="black")
    } else{points(occ, pch=1, cex=0.5, col="black")}
  dev.off()
  
  return(rastpreds)
  
} ## end of main function

