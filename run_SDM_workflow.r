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
              "rgdal", "mgcv", "PresenceAbsence", "doParallel") #list of required packages

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])] #check which of them is not yet installed
if(length(new.packages)) install.packages(new.packages); rm(new.packages) #install them

l <- sapply(packages, require, character.only = TRUE); rm(packages, l) #load all reaquired packages

##########################################################################################################
## create outputfolder ###################################################################################

dir.create("output")


##########################################################################################################
## everything the user needs to specify ##################################################################

identifier <- "run.270521.Lj" #a unique identifier for every run of the SDM workflow, needs to be a character

myspecies <- "Lonicera japonica" #focal species, user should insert the scientific name as character string in the shown format here

occ <- read.csv("occtemp.csv")[-1] #alternatively, the user can insert a table with occurrence records here. This table should be in a .csv-format and be formatted exactly like the example in the manual; if the user does not run the entire analysis at once, she/he can insert the GBIF occurrence table created in  the first step here. #when reading in the existent-file, it is important to remove the first column!

envir <- c("bio1", "bio5") #environmental variables of choice, user should insert the names of the desired variables as character string as shown here

filecrop <-c(NULL) #optional:the extent to which the area for model fitting (!) should be cropped; set to NULL for global extent
#note: this is not yet implemented in the functions

filecropbase <- c(-30,70,30,90) #optional:the extent to which the area for model predictions (!) should be cropped; set to NULL for global extent

load(file.path("output", paste0("PAlist",identifier,".RData"))) #optional: the user can load the PA list here, if she/he has already run the pseudabsence selection

load(file.path("output", paste0("suitability",identifier,".RData")))


##########################################################################################################
## load functions ########################################################################################

source("load_functions.r")


##########################################################################################################
## prepare data ##########################################################################################

if(exists("occ")){userdatacheck(occ)} else{occ <- GBIFdownclean(myspecies)} # checks quality of occurrence data of the user, alternatively downloads, cleans and plots GBIF occurrence data

occenv <- addenvir2(envir) #loads desired environmental variables, checks for correlation of these variables acorss the study region, extracts environmental variables for the occurrence records. If the correlation among environmental variables is too high, the user needs to remove them from envir and run this step here again

if(!exists("PAlist")){PAlist <- PAsampleParallel(occenv, envir)} #samples 10 sets of pseudoabsences, extracts environment info for the pseudoabsences, attaches everything to the occurence table, creates a list with the ten occurrence-pseudabsence datasets 

save(PAlist, file=file.path("output", paste0("PAlist",identifier,".RData")))
# optional: save PA list as .RData


##########################################################################################################
## fit and validate models ###############################################################################

modelruns100 <- GAMfitting(PAlist) # splits each pseudoabsence dataset into 10 random 30-70 datasplits and fits and evaluates one GAM with each data split, returns an object called modelruns100


##########################################################################################################
## predict suitability ###################################################################################

baseenvir <- loadBaseEnv(envir, filecropbase) #loads the environmental layers of interest and crops them to the study area

suitability <- predictGAMParallel(baseenvir, modelruns100) #predicts environmental suitability based on models with a sufficiently good quality (AUC > 0.7)

save(suitability, file=file.path("output", paste0("suitability",identifier,".RData"))) #optional: save list with suitability predictions for the different model runs

avgsuitability <- modelaverageParallel(suitability, envir) #averages the predicted anvironmental suitability across the different models and saves a csv-file with the average suitabilities over the model runs

rasterSuitabilities <- plotSuitabilities(avgsuitability,occ) #transforms the average suitabilities to a raster file, plots them, saves the plot as pdf and returns the raster file
