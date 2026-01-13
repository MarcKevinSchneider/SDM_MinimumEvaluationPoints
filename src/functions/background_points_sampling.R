#' @name background_points_sampling.R
#' @date 13.01.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Function for sampling the background points

# ================================================================
# 1. Load setup script
# ================================================================

rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = FALSE) # echo set to false here to stop the script from printing

# set seed
set.seed(2962)

# ================================================================
# 2. 10,000 Random Background Points Sampling function
# ================================================================

bck_sampling <- function(species_name, fit){
  '
  Purpose: Samples one 10,000 background points dataset per species and fit
  
  
  Parameters:
  ----------------------------
  
  species_name: str
    Name of the species; example: "VS01", "VS02"
    
  fit: str
    Name of the goodness of fit; example: "0.1", "0.2"
    
  
  Returns:
  ---------------------------
  A random 10,000 background points dataset
  
  '
  
  # 1. Reading the ADM data
  #--------------------------------------------------------
  
  landscape <- terra::rast(paste0(envrmt$path_ADM, "/", species_name, "/", species_name,
                                  "_", "Fit_", fit, ".tif"))
  
  # 2. Sampling the 10,000 background points
  #--------------------------------------------------------
  
  background_points <- sf::st_as_sf(as.data.frame(predicts::backgroundSample(mask=landscape, n=10000)), 
                                    crs=terra::crs(landscape), coords=c("x","y"), remove=F)
  
  # 3. Saving the dataset
  #--------------------------------------------------------
  
  # extracting the data for the background points
  bg_extr <- terra::extract(landscape, background_points)
  background_points <- cbind(background_points,bg_extr);rm(bg_extr)
  
  # creating directory for the background data
  dir_bkg <- paste0(envrmt$path_bkg_points, "/Random/", species_name, "/")
  if(!dir.exists(dir_bkg)) dir.create(dir_bkg, recursive = TRUE)
  # saving the background data
  sf::write_sf(background_points, paste0(dir_bkg, "/", species_name, "_Fit_",
                                         fit, "_Background.gpkg"))
}


# ================================================================
# 3. Balanced Random Background Points Sampling function
# ================================================================

balanced_bck_sampling <- function(species_name, fit, sample_p, iter){
  '
  Purpose: Samples a specified number of background points randomly
  
  
  Parameters:
  ----------------------------
  
  species_name: str
    Name of the species; example: "VS01", "VS02"
    
  fit: str
    Name of the goodness of fit; example: "0.1", "0.2"
    
  sample_p: int
    Number of sample points
    
  iter: str
    What iteration the code is on
    
  
  Returns:
  ---------------------------
  A random background points dataset with a specified amount of points
  
  '
  
  # 1. Reading the ADM data
  #--------------------------------------------------------
  
  landscape <- terra::rast(paste0(envrmt$path_ADM, "/", species_name, "/", species_name,
                                  "_", "Fit_", fit, ".tif"))
  
  # 2. Sampling the specified amount of points
  #--------------------------------------------------------
  
  background_points <- sf::st_as_sf(as.data.frame(predicts::backgroundSample(mask=landscape, n=sample_p)), 
                                    crs=terra::crs(landscape), coords=c("x","y"), remove=F)
  
  # 3. Saving the dataset
  #--------------------------------------------------------
  
  # extracting the data for the background points
  bg_extr <- terra::extract(landscape, background_points)
  background_points <- cbind(background_points,bg_extr);rm(bg_extr)
  
  # creating directory for the background data
  dir_bkg <- paste0(envrmt$path_bkg_points, "/Balanced/", species_name, "/", sample_p)
  if(!dir.exists(dir_bkg)) dir.create(dir_bkg, recursive = TRUE)
  # saving the background data
  sf::write_sf(background_points, paste0(dir_bkg, "/", species_name, "_Fit_",
                                         fit, "_Iteration_", iter, "_Background.gpkg"))
}