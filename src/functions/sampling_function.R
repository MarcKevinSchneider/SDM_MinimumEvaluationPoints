#' @name sampling_function.R
#' @date 09.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for sampling the presence points, absence points and presence points

# ================================================================
# 1. Load setup script
# ================================================================

rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/Minimum_Evaluation_Points_SDM/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = FALSE) # echo set to false here to stop the script from printing

set.seed(2962)

# ================================================================
# 2. Sampling functions
# ================================================================


# 1 - Function for random sampling of presence-absence and background data ####
#-----------------------------------------#

random_sampling <- function(species_name, fit, sample_p, iter){
  
  fit <- as.character(fit)
  
  # reading the species and landscape data
  species <- readRDS(paste0(envrmt$path_VirtualSpecies, "/", species_name, ".rds"))
  landscape <- terra::rast(paste0(envrmt$path_ADM, "/", species_name, "/", species_name,
                                  "_", "Fit_", fit, ".tif"))
  
  # extracting the occurrence data
  presence <- terra::unwrap(species[[4]])
  
  #print(presence)
  #plot(presence)
  
  # multiply by two to get equal amounts of presence and absence points
  sample_p_2 = sample_p * 2
  
  # sampling the same amount of presence and absence data points
  pres_abs_points <- sampleOccurrences(presence, n=sample_p_2, 
                                       type="presence-absence", replacement=FALSE,
                                       sample.prevalence=0.5)
  #print(pres_abs_points)
  # convert to dataframe
  pres_abs_df <- as.data.frame(pres_abs_points$sample.points)
  # convert to stars vector object
  #print(pres_abs_df)
  
  pres_abs_sf <- sf::st_as_sf(pres_abs_df, coords = c("x", "y"), 
                              crs = terra::crs(landscape),remove = F)
  
  
  #sampling the 10,000 bkg points
  background_points <- sf::st_as_sf(as.data.frame(predicts::backgroundSample(mask=landscape, n=10000)), 
                                    crs=terra::crs(landscape), coords=c("x","y"), remove=F)
  
  #extracting the data for the background points
  bg_extr=terra::extract(landscape, background_points)
  background_points =cbind(background_points,bg_extr);rm(bg_extr)
  
  #extracting the data for the presence-points
  species_data_extr <- terra::extract(landscape, pres_abs_sf)
  species_data_compl <- cbind(pres_abs_sf, species_data_extr)
  #print(species_data_compl)
  
  sample_p <- as.character(sample_p)
  
  # creating directory for the presence absence data
  dir_pres <- paste0(envrmt$path_pre_abs_points, "/", "Random", "/", species_name, "/", sample_p)
  if(!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  # saving the presence absence data
  sf::write_sf(species_data_compl, paste0(dir_pres, "/", species_name, "_Fit_", 
                                          fit, "_Iteration_", iter, "_Pres_Abs.gpkg"))
  # creating directory for the background data
  dir_bkg <- paste0(envrmt$path_bkg_points, "/", "Random", "/", species_name, "/", sample_p)
  if(!dir.exists(dir_bkg)) dir.create(dir_bkg, recursive = TRUE)
  # saving the background data
  sf::write_sf(background_points, paste0(dir_bkg, "/", species_name, "_Fit_",
                                         fit, "_Iteration_", iter, "_Background.gpkg"))
  print(paste0("Saved species data for n=", sample_p, "!"))
}
  