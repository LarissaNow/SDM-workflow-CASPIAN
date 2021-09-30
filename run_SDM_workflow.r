###########################################################################################################
#
# This script runs the SDM workflow for automated prediction of environmental suitabaility of a focal area
# for a focal species
# This workflow can be combined with CASPIAN
#
# Author: Larissa Nowak
##########################################################################################################



##########################################################################################################
## Automatically install and load required packages, which are not yet in library ########################
## source: https://github.com/christianhof/BioScen1.5_SDM/blob/master/R

packages <- c("raster", "rgbif", "CoordinateCleaner", "maps", "dismo", "ggplot2", 
              "rgdal", "mgcv", "PresenceAbsence", "doParallel", "viridis") # list of required packages

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])] # check which of them is not yet installed
if(length(new.packages)) install.packages(new.packages); rm(new.packages) # install them

l <- sapply(packages, require, character.only = TRUE); rm(packages, l) # load all required packages


##########################################################################################################
## create outputfolder ###################################################################################

dir.create("output")


##########################################################################################################
## everything the user needs to specify ##################################################################

identifier <- "run.190921.Senecio.no.prec." # a unique identifier for every run of the SDM workflow, needs to be a character

myspecies <- "Senecio inaequidens" # focal species, user should insert the scientific name as character string in the format shown here

occ <- read.csv("occtemp.csv")[-1] # alternatively, the user can insert a table with occurrence records here. This table should be in a .csv-format and be formatted exactly like the example in the manual; if the user does not run the entire analysis at once, she/he can insert the GBIF occurrence table created in  the first step here. 
# when reading in the file created during this workflow, it is important to remove the first column by adding [-1] as shown here!

envir <- c("bio1") # environmental variables of choice, user should insert the names of the desired variables as character as shown here

landcov <- c("LC2", "LC12") # land cover variables of choice, user should insert the names of the desired variables as character as shown here

filecrop <-c(NULL) # optional:the extent to which the area for model fitting (!) should be cropped; set to NULL for global/European extent
# note: this is not yet implemented in the functions

filecropbase <- c(NULL) # optional:the extent to which the area for model predictions (!) should be cropped; set to NULL for global/European extent

GermanShapefile <- readOGR(dsn=getwd(),layer="gadm28_adm2_Germany") # optional: loads a shapefile of the shape of Germany to be used for cropping the suitability plot to the extent and shape of Germany; set to NULL if this is not desired!

load(file.path("output", paste0("PAlist",identifier,".RData"))) # optional: the user can load the PA list here, if she/he has already run the pseudabsence selection

load(file.path("output", paste0("suitability",identifier,".RData"))) # optional: the user can load the environmental suitability list here, if she/he has already computed the suitabilities

netshp <- readOGR(dsn=getwd(), layer = "RailRoadNetw_Intersection_100519") # read in the traffic network shapefile that is utiized in the CASPIAN workflow
# note: in the long run this bit will be separated from the SDM-workflow and will be part of the traffic-network-block of the entire work package

##########################################################################################################
## load functions ########################################################################################

source("load_functions.r") # this loads all functions that are required throughout the SDM workflow


##########################################################################################################
## prepare data ##########################################################################################

if(exists("occ")){userdatacheck(occ)} else{occ <- GBIFdownclean(myspecies)} # checks quality of occurrence data of the user, alternatively downloads, cleans and plots GBIF occurrence data

if(!is.null(landcov)){occenv <- addenvirLC2(envir,landcov)} else{occenv <- addenvir2(envir)} # loads desired environmental variables, checks for correlation of these variables accross the study region, extracts environmental variables for the occurrence records. If the correlation among environmental variables is too high, the user needs to remove them from envir and run this step again

if(!is.null(landcov)){if(!exists("PAlist")){PAlist <- PAsampleParallelLC(occenv, envir, landcov)}} else{if(!exists("PAlist")){PAlist <- PAsampleParallel(occenv, envir)}} # samples 10 sets of pseudoabsences, extracts environment info for the pseudoabsences, attaches everything to the occurence table, creates a list with the ten occurrence-pseudabsence datasets 

save(PAlist, file=file.path("output", paste0("PAlist",identifier,".RData")))
# optional: save PA list as .RData


##########################################################################################################
## fit and validate models ###############################################################################

if(!is.null(landcov)){modelruns100 <- GAMfittingLC2(PAlist)} else{modelruns100 <- GAMfitting(PAlist)} # splits each pseudoabsence dataset into 10 random 30-70 datasplits and fits and evaluates one GAM with each data split, returns an object called modelruns100


##########################################################################################################
## predict suitability ###################################################################################

if(!is.null(landcov)){baseenvir <- loadBaseEnvLC2(envir, filecropbase, landcov)} else{baseenvir <- loadBaseEnv(envir, filecropbase)} # loads the environmental layers of interest and crops them to the study area

suitability <- predictGAMParallel(baseenvir, modelruns100) # predicts environmental suitability based on models with a sufficiently good quality (AUC > 0.7)

save(suitability, file=file.path("output", paste0("suitability",identifier,".RData"))) # optional: save list with suitability predictions for the different model runs

if(!is.null(landcov)){avgsuitability <- modelaverageParallelLC(suitability, envir, landcov)} else{avgsuitability <- modelaverageParallel(suitability, envir)} # averages the predicted environmental suitability across the different models and saves a csv-file with the average suitabilities over the model runs

rasterSuitabilities <- plotSuitabilities(avgsuitability,occ,GermanShapefile) # transforms the average suitabilities to a raster file, plots them, saves the plot as pdf and returns the raster file
# NOTE: If the same plot should be plotted and stored again, make sure the pdf-file with the respective name is closed. Otherwise, R will be unable to overwrite the file and yield an error, when running this step.

if(!is.null(GermanShapefile)){netshp<-netshp[GermanShapefile,]} # this step subsets the traffic network to the shape and extent of Germany, this makes sense, if the plotting and creating of the suitability raster file in the previous step has been done at the extent of Germany, too

trafficnet <- SuitabilityNet(netshp, rasterSuitabilities) # computes the mean and standard deviation of environmental suitabilities for each link (line segment) in the traffic network, returns a shapefile
# note: in the long run this bit will be separated from the SDM-workflow and will be part of the traffic-network-block of the entire work package

writeOGR(trafficnet, dsn="output", layer=paste0("Traffic_net_env_",identifier), driver="ESRI Shapefile") # saves the shape file from the previous step
# note: in the long run this bit will be separated from the SDM-workflow and will be part of the traffic-network-block of the entire work package

spplot(trafficnet[,"Env_suit"]) # this creates a plot of this shapefile colored according to the suitabilities.
# note: in the long run this bit will be separated from the SDM-workflow and will be part of the traffic-network-block of the entire work package
