#' @name systematic_sampling_function.R
#' @date 19.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for systematic sampling the presence points and absence points

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
# 2. Systematic Sampling function
# ================================================================

systematic_sampling <- function(species_name, fit, sample_p, iter){
  '
  Purpose: Samples presence-absence points using a precomputed systematic grid
           e.g. it only samples every n-th pixel
  
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
  A presence-absence dataset with a systematic sampling approach
  '
  
  # 1. Ensuring data structure
  species_name <- as.character(species_name)
  fit <- as.character(fit)
  sample_p <- as.numeric(sample_p)
  iter <- as.character(iter)
  
  # 2. Read species raster and landscape
  species <- readRDS(paste0(envrmt$path_VirtualSpecies, "/", species_name, ".rds"))
  landscape <- terra::rast(paste0(envrmt$path_ADM, "/", species_name, "/", species_name,
                                  "_Fit_", fit, ".tif"))
  presence <- terra::unwrap(species[[4]])  # occurrence layer
  
  # multiply by two to get equal amounts of presence and absence points
  sample_p_2 = sample_p * 2
  
  # 3. Load the precomputed systematic grid for this sample size
  grid_file <- paste0(envrmt$path_systematic_grids, "/", species_name, "/", species_name,
                      "_systematic_grid_n_", sample_p, ".gpkg")
  grid_pts <- sf::st_read(grid_file, quiet = TRUE)
  #print("Read the grid...")
  
  grid_centers <- sf::st_centroid(grid_pts)
  
  
  # sampling the same amount of presence and absence data points
  pres_abs_points <- sampleOccurrences(presence, n=sample_p_2, 
                                       type="presence-absence", replacement=FALSE,
                                       sample.prevalence=0.5, 
                                       sampling.area = grid_centers, 
                                       plot=TRUE)
  
  # 5. Formatting the presence data
  #--------------------------------------------------------
  
  # convert to dataframe
  pres_abs_df <- as.data.frame(pres_abs_points$sample.points)
  
  # convert to stars vector object
  pres_abs_sf <- sf::st_as_sf(pres_abs_df, coords = c("x", "y"), 
                              crs = terra::crs(landscape),remove = F)
  
  
  # 6. Extracting the landscape data
  #--------------------------------------------------------
  
  # extracting the data for the presence-points
  species_data_extr <- terra::extract(landscape, pres_abs_sf)
  species_data_compl <- cbind(pres_abs_sf, species_data_extr)
  #print(species_data_compl)
  
  sample_p <- as.character(sample_p)
  
  # 7. Saving the data
  #--------------------------------------------------------
  
  # creating directory for the presence absence data
  dir_pres <- paste0(envrmt$path_pre_abs_points, "/Systematic/", species_name, "/", sample_p)
  if(!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  # saving the presence absence data
  sf::write_sf(species_data_compl, paste0(dir_pres, "/", species_name, "_Fit_", 
                                          fit, "_Iteration_", iter, "_Pres_Abs.gpkg"))
  #print(paste0("Saved species data for n=", sample_p, "!"))
  
}