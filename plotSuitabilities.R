###########################################################################################################
#
# Function to plot the average predicted environmental suitabilities
# This function is part of the SDM workflow.
#
# Author: Larissa Nowak
##########################################################################################################

plotSuitabilities <- function(avgsuitability, occ, GermanShapefile) { ## start of main function
  
  print("Note: If the same plot should be plotted and stored again, make sure the pdf-file with the respective name is closed on your computer. Otherwise, R will be unable to overwrite the file and yield an error, when running this step.") # notification for the user
  
  avgsuitability2 <- avgsuitability[,c("x", "y", "modelaverage")] # prepare raster file with the mean predictions for plotting
  coordinates(avgsuitability2) <- ~ x + y
  gridded(avgsuitability2) <- T
  rastpreds <- raster(avgsuitability2)
  
  
  coordinates(occ)=~decimalLongitude+decimalLatitude # prepare occurrence file for plotting, transform it into a shapefile
  proj4string(occ)<- CRS("+proj=longlat +datum=WGS84")
  
  if(!is.null(GermanShapefile)){ # if the user wants the plot to be cropped to the extend and shape of Germany
    rastpreds <- mask(rastpreds,GermanShapefile) # that is how to subset a raster file
    occ <- occ[GermanShapefile,] # that is how to subset a shapefile
    xlim <- c(5,15)
    ylim <- c(47,56)} else{xlim <- NULL
                          ylim <- NULL}
  
  par(bty="l")
  
  plot(rastpreds, col=viridis(100), xlim=xlim, ylim=ylim) # plot the predicted probabilities
  
  if(!is.null(filecropbase)) {points(occ, pch=1, cex=0.5, xlim=filecropbase[1:2], ylim=filecropbase[3:4], col="black", xlim=xlim, ylim=ylim)
    } else{points(occ, pch=1, cex=0.5, col="black", xlim=xlim, ylim=ylim)} # plot occurrences
  
  # store plots on the user computer:
  pdf(file.path("output", paste0("Suitability_",identifier,".pdf"))) # plot without occurrences
  plot(rastpreds, col=viridis(100), xlim=xlim, ylim=ylim)
  dev.off()
 
   pdf(file.path("output", paste0("SuitabilityOccurrences_",identifier,".pdf"))) # plot with occurrences
  plot(rastpreds, col=viridis(100), xlim=xlim, ylim=ylim)
  if(!is.null(filecropbase)) {
    points(occ, pch=1, cex=0.5, xlim=filecropbase[1:2], ylim=filecropbase[3:4], col="black", xlim=xlim, ylim=ylim)
    } else{points(occ, pch=1, cex=0.5, col="black", xlim=xlim, ylim=ylim)}
  dev.off()
  
  return(rastpreds)
  
  print("Note: If the rasterfile is cropped to Germany for plotting it will also be returned in a cropped manner from this function.") 
  
} ## end of main function

