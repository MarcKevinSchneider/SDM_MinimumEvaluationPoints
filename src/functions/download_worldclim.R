#' @name download_worldclim.R
#' @date 09.12.2025
#' @author Marc Kevin Schneider & Lisa Bald
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Downloads the worldclim data for the study area of the analysis.
#' 
#' Original code by Lisa Bald (contact: bald@staff.uni-marburg.de). Adjusted by
#' Marc Kevin Schneider.

# ================================================================
# 1. Load setup script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# ================================================================
# 2. Download the data
# ================================================================

# checks if the bioclim file exists
if (!file.exists(paste0(envrmt$path_data, "/variables.tif"))){
  # if not download the bioclim datan for australia
  bioclim=geodata::worldclim_country(country="Australia", path="data", var="bio", res=0.5)
  
  names(bioclim) <-  substr(names(bioclim), start=11, stop=16)
  
  # subset to only those 4 bands
  bioclim=terra::subset(bioclim, c("bio_1", "bio_3", "bio_7", "bio_12"))
  
  
  # download the border data for Australia
  border=geodata::gadm(country="Australia", path="data")
  # converr to simple feature
  border=sf::st_as_sf(border)
  # filter for NWS, Vic and ACT
  border=border%>%dplyr::filter(NAME_1 %in% c("New South Wales", "Victoria", 
                                              "Australian Capital Territory"))
  # transform to same CRS
  border=sf::st_transform(border, terra::crs(bioclim))
  
  # clip and mask the bioclim data
  bioclim=terra::crop(bioclim, border)
  bioclim=terra::mask(bioclim, border)
  # project to epsg 3577 (GDA94 / Australian Albers)
  bioclim=terra::project(bioclim, "epsg:3577")
  
  # write to a tif file
  terra::writeRaster(bioclim, paste0(envrmt$path_data, "/variables.tif"), overwrite=T)
  rm(border, bioclim)
}
