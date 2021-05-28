###########################################################################################################
#
# This script loads all functions needed for the SDM workflow
#
# Author: Larissa Nowak
##########################################################################################################


########################################################################
### load scripts #######################################################

# data preparation
source("userdatacheck.r")
source("GBIFdownclean.r")
source("addenvir2.r")
source("PAsampleParallel.r")

# model fitting and validation
source("GAMfitting.r")

# suitability prediction
source("loadBaseEnv.r")
source("predictGAMParallel.r")
source("modelaverageParallel.r")
source("plotSuitabilities.r")