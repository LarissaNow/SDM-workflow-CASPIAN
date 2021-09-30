###########################################################################################################
#
# This script loads all functions needed for the SDM workflow
#
# Author: Larissa Nowak
##########################################################################################################


###########################################################################
### load functionss #######################################################

# data preparation
source("userdatacheck.r")
source("GBIFdownclean.r")
source("addenvir2.r")
source("addenvirLC2.r")
source("PAsampleParallel.r")
source("PAsampleParallelLC.r")

# model fitting and validation
source("GAMfitting.r")
source("GAMfittingLC2.r")

# suitability prediction
source("loadBaseEnv.r")
source("loadBaseEnvLC2.r")
source("predictGAMParallel.r")
source("modelaverageParallel.r")
source("modelaverageParallelLC.r")
source("plotSuitabilities.r")
source("SuitabilityNet.r")