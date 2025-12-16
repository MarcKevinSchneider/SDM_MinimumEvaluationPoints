#' @name 00_setup_project.R
#' @date 08.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Setup script for the analysis "Establishing the Minimum Number of Validation 
#' Points Needed for Reliable Species Distribution Model Assessment 
#' Under Varying Conditions". 
#' 
#' Sets the folder structure and loads the most important packages.


# ================================================================
# 1. Load required packages
# ================================================================

library(envimaR)          # for the folder structure
library(dplyr)            # data manipulation
library(sf)               # spatial vector data
library(parallel)         # parallel processing
library(RandomFields)     # Gaussian random fields
library(NLMR)             # neutral landscape models
library(terra)            # raster handling
library(climateStability) # rescaling to [0,1]
library(RandomFieldsUtils)# dependency RandomField package
library(raster)           # dependency RandomField package
library(virtualspecies)   # for virtual species
library(ggplot2)          # for plotting
library(blockCV)          # for some of the sampling strategies
library(Metrics)          # for the evaluation metrics 
library(tidyverse)        # general functions
library(geodata)          # for the download of the border data

# ================================================================
# 2. Folder structure
# ================================================================

# set root directory
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"

# folder structure
projectDirList <- c(
  "data/",
  "data/paRaster",            # Presence-Absence Rasters
  "data/VirtualSpecies",      # Virtual Species RDS files
  "data/ADM",                 # Artificial Distribution Maps
  "data/pre_abs_points",      # Presence-Absence Points RDS files
  "data/bkg_points",          # Background Points RDS files
  "docs/",
  "run/",
  "tmp",
  "src/",
  "src/functions/"
)

# append additional folders if defined by calling script
if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") {
  projectDirList <- append(projectDirList, appendProjectDirList)
}

#print(rootDir)

# Automatically set root directory, folder structure and load libraries
envrmt <- envimaR::createEnvi(
  root_folder = rootDir,
  folders = projectDirList,
  path_prefix = "path_",
  alt_env_id = "COMPUTERNAME",
  alt_env_value = "PCRZP",
  alt_env_root_folder = "F:/BEN/edu"
)
