#################################################################################
#
# Function to extract the suitabilities for each link in the traffic network
# and to compute the mean suitability per link. This can then be used for CASPIAN.
#
# Author: Larissa Nowak
#################################################################################

SuitabilityNet <- function(netshp, rasterSuitabilities){ ## START of main function

  coordnet <- coordinates(netshp) # exctract the coordinates of the links (line segments) in the shapefile

  emptylist <- list() # create an empty list to be filled along the way

  n=1 # numerator required for creating list elements

  for (i in coordnet) { # extract suitabilities for each link (i.e. line segment)
    emptylist[[n]] <- extract(rasterSuitabilities,i[[1]])
    n<-n+1}

  suitabs <- as.matrix(sapply(emptylist,FUN=mean)) # compute the mean accross the suitabilities along the link

  netshp["Env_suit"] <- suitabs # add the mean suitabilities to the attributes data frame of the shapefile
  # this step requires that the coordinate matrices are given in the same order as the attributes in the attribute table of the shapefile!!!

  suitabsSD <- as.matrix(sapply(emptylist,FUN=sd)) # compute the standard deviation accross the suitabilities along the link

  netshp["Env_suit_SD"] <- suitabsSD # add the standard deviation of the suitabilities to the attributes data frame of the shapefile

  return(netshp) # rwturn the input shapefile with the newly addd data

  } ## END of main function

# What needs to be added: the link between each line segment here and its ID in the datatable so that the mean suitabilities can be added to the data table
