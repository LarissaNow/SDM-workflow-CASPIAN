#######################################################################################
# 
# Script to transform Corine land cover data
# The data is loaded, reprojected, aggreagted and transformed to percentage land cover, 
# resampled to match Worldclim data and saved as raster files. 
# Parts of this code are adapted from: 
# https://damariszurell.github.io/HU-GCIB/7_SDM_conservation.html#23_land_cover_data
# 
# Author: Larissa Nowak
#######################################################################################

###############
# 1 # Load data
###############

file2012 <- raster("C:\\Users\\Larissa\\Dropbox\\Larre\\PostDoc\\Invasive_Hanno\\SDMs\\Habitat data\\3bafef4f5489fe0c5ddc2674daef46d3f915e459 (2)\\u2018_clc2012_v2020_20u1_raster100m\\u2018_clc2012_v2020_20u1_raster100m\\DATA\\U2018_CLC2012_V2020_20u1.tif")

plot(file2012)

# Issue: Warning message:
# In showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
# Discarded datum Unknown based on GRS80 ellipsoid in Proj4 definition

#########################################
# 2 # Reproject to match the climate data
#########################################

Template <- raster("wc2.1_2.5m_bio_1.tif") # template to take the projection from

Repro <- projectRaster(file2012, crs=crs(Template), method="ngb") # 'ngb' nearest neighbour method, because it is categorical data

plot(Repro)

#######################################################################
# 3 # Aggreagte the LC data to large grids and to percentage land cover
#######################################################################
# # # Resample the land cover data to match the climate data
############################################################

levelsLC <- sort(unique(values(Repro)))

for (i in levelsLC){ # for each land cover type
  print(i)
  temp_r <- aggregate(Repro,c(30,46), fun=function(x,...){sum(x==i)/1380}) # aggregate: compute the percentage cover
  temp_r2 <- resample(temp_r, Template) # resample to match the resolution of the climate layer
  temp_r3 <- crop(temp_r2, temp_r) # crop (because climate layer is global, while land cover layer is Europe)
  names(temp_r) <- paste0("LC",as.character(i)) # name the layer
  if (i==1) {lc <- temp_r} else {lc <- addLayer(lc, temp_r)}} # add the layer to the raster stack

writeRaster(lc, filename=names(lc), bylayer=TRUE,format="GTiff") # save the final output; this will save each layer as a separate raster file


