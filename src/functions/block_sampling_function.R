#' @name block_sampling_function.R
#' @date 16.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for block sampling the presence points and absence points

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
# 2. Block Sampling function
# ================================================================

block_sampling <- function(species_name, fit, sample_p, iter){
  '
  Purpose: Samples presence-absence points using a spatial blocking approach
  
  
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
  A presence-absence dataset with spatial blocking and a random background dataset
  
  '
  
  # 1. Ensuring data structure
  #--------------------------------------------------------
  species_name <- as.character(species_name)
  fit <- as.character(fit)
  sample_p <- as.numeric(sample_p)
  iter <- as.character(iter)
  
  
  # 2. Correctly reading and preparing the data
  #--------------------------------------------------------
  
  # reading the species and landscape data
  species <- readRDS(paste0(envrmt$path_VirtualSpecies, "/", species_name, ".rds"))
  landscape <- terra::rast(paste0(envrmt$path_ADM, "/", species_name, "/", species_name,
                                  "_", "Fit_", fit, ".tif"))
  
  # extracting the occurrence data
  presence <- terra::unwrap(species[[4]])
  
  # multiply by two to get equal amounts of presence and absence points
  sample_p_2 = sample_p * 2
  
  
  # 3. Sampling the background data
  #--------------------------------------------------------
  
  # sampling the 10,000 bkg points
  background_points <- sf::st_as_sf(as.data.frame(predicts::backgroundSample(mask=landscape, n=10000)), 
                                    crs=terra::crs(landscape), coords=c("x","y"), remove=F)
  
  # 4. Choosing the blocking strategy
  #--------------------------------------------------------
  # random number between 1 and 3 to determine whether bars, squares or hexagons
  # should be used for the block sampling
  random_num <- as.numeric(sample(1:3, 1))
  
  if (random_num == 1){
    # if number is one then it just uses squares 
    blocks <- blockCV::cv_spatial(r = landscape,
                                  hexagon = FALSE,
                                  x = background_points,
                                  k = 5)
  } else if (random_num == 2){
    # if number is two then use horizontal or vertical blocking
    # in this case a number between one and two is chosen
    col_row <- as.numeric(sample(1:2, 1))
    
    if (col_row == 1){
      # if number is 1 then use 5 horizontal rows
      blocks <- blockCV::cv_spatial(r = landscape,
                                    rows_cols = c(5, 0),
                                    hexagon = F, 
                                    x = background_points)
    } else {
      # if number is 2 then use 5 vertical columns
      blocks <- blockCV::cv_spatial(r = landscape,
                                    rows_cols = c(0, 5),
                                    hexagon = F, 
                                    x = background_points)
    }
  } else if (random_num == 3){
    # if number is three then use hexagons with a random size between 200 and 1000
    # random hexagon size
    hex_size <- as.numeric(sample(c(200, 400, 600, 800, 1000), size=1))
    blocks <- blockCV::cv_spatial(r = landscape,
                                  size = hex_size, # random hexagon size 
                                  hexagon = T, # use hexagons
                                  x = background_points,
                                  k = 5)
  }
  
  # 5. Sampling the occurrence data from the selected folds
  #--------------------------------------------------------
  
  # randomly select three folds that get sampled
  # so two folds are left out which are not sampled
  random_samp_area <- as.numeric(sample(1:5, 3))
  samplingArea = blocks$blocks %>% dplyr::filter(folds %in% random_samp_area)
  
  # sampling the same amount of presence and absence data points
  # specifying the sampling area here so that we only sample in the folds we selected
  pres_abs_points <- sampleOccurrences(presence, n=sample_p_2, 
                                       type="presence-absence", replacement=FALSE,
                                       sample.prevalence=0.5, 
                                       sampling.area = samplingArea)
  
  
  # 6. Final formatting of the data
  #--------------------------------------------------------
  
  # convert to dataframe
  pres_abs_df <- as.data.frame(pres_abs_points$sample.points)
  
  # convert to stars vector object
  pres_abs_sf <- sf::st_as_sf(pres_abs_df, coords = c("x", "y"), 
                              crs = terra::crs(landscape),remove = F)
  
  # extracting the data for the background points
  bg_extr <- terra::extract(landscape, background_points)
  background_points <- cbind(background_points,bg_extr);rm(bg_extr)
  
  # extracting the data for the presence-points
  species_data_extr <- terra::extract(landscape, pres_abs_sf)
  species_data_compl <- cbind(pres_abs_sf, species_data_extr)
  #print(species_data_compl)
  
  sample_p <- as.character(sample_p)
  
  # 7. Saving the data
  #--------------------------------------------------------
  
  # creating directory for the presence absence data
  dir_pres <- paste0(envrmt$path_pre_abs_points, "/Block/", species_name, "/", sample_p)
  if(!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  # saving the presence absence data
  sf::write_sf(species_data_compl, paste0(dir_pres, "/", species_name, "_Fit_", 
                                          fit, "_Iteration_", iter, "_Pres_Abs.gpkg"))
  # creating directory for the background data
  dir_bkg <- paste0(envrmt$path_bkg_points, "/Block/", species_name, "/", sample_p)
  if(!dir.exists(dir_bkg)) dir.create(dir_bkg, recursive = TRUE)
  # saving the background data
  sf::write_sf(background_points, paste0(dir_bkg, "/", species_name, "_Fit_",
                                         fit, "_Iteration_", iter, "_Background.gpkg"))
  #print(paste0("Saved species data for n=", sample_p, "!"))
}