###########################################################################################################
#
# Function to check whether the occurrence data presented by the user has sufficient occurrence records 
# This function is part of the SDM workflow. 
#
# Author: Larissa Nowak
##########################################################################################################

userdatacheck <- function(occ) { ## start of main function
  
  noofocc <- length(occ$Presence==1) 
  
  if(noofocc < 50) {print("NOTE: The number of occurrences is < 50. SDMs might not yield reliable results. Consider downloading GBIF data.")} # notification for the user
  
  } ## end of main function
