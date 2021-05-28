###########################################################################################################
#
# Function to automatically download, clean and check the quality of GBIF occurrence records 
# for a given focal species 
# This function is part of the SDM workflow. 
#
# Author: Larissa Nowak
##########################################################################################################

GBIFdownclean <- function(myspecies) { ## start of main function
  
  print("NOTE: The download of the GBIF data might take several minutes.") # notification for user
  
  gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 100000) 
  # download of GBIF occurrences, note: this is limited to 100000 occurrences
 
  
  testGBIF <- suppressMessages(suppressWarnings(clean_coordinates(gbif_data$data, lon = "decimalLongitude",
                                                                  lat = "decimalLatitude",
                                                                  countries = "countryCode",
                                                                  species = "species"))) 
  # cleaning of the occurrence records using the function clean_coordinates from the package CoordinateCleaner
  
  myspecies_coords <- gbif_data$data[testGBIF$.summary,] # subsetting the GBIF data to the clean records
  
  noofocc <- nrow(myspecies_coords) # check number of occurrences
  
  if(noofocc < 50) { # check, whether the number of occurrences is sufficient
    print("NOTE: The number of occurrences is < 50. SDMs might not yield reliable results.") }
  
  else{
    
    par(mfrow=c(1,1)) # adjust plotting window
    map("world") # plot world map
    points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], pch = 1, cex = 0.3, col = "darkgreen") # plot cleaned occurrence points #
    
    occ <- as.data.frame(myspecies_coords[c("decimalLongitude", "decimalLatitude", "occurrenceStatus")]) 
    # subset the occurrence records to the columns needed for the modelling
    
    occ[which(occ$occurrenceStatus=="PRESENT") , "Presence"] <- 1 # add a presence (0/1) column to this data frame
    
    occ$occurrenceStatus <- NULL # the final occurrence file
    
    write.csv(occ, "occtemp.csv") # stores the final occurrence file on the users computer
    
    print("NOTE: 'occtemp' has been saved on your computer as csv-file. This file contains the GBIF occurrences of the target species. If required it can be edited, e.g. to manually remove outliers. After editing, it needs to be read into the variable 'occ'." ) # notification for the user
    
    return(occ)}
  
} ## end of main function