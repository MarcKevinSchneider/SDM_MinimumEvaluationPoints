#' @name effortDriven_sampling_function.R
#' @date 23.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for effort driven sampling the presence points and absence points

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
# 2. Effort Driven Sampling function
# ================================================================

effort_sampling <- function(species_name, fit, sample_p, iter){
  '
  Purpose: Samples presence-absence points using an effort-driven sampling approach,
           e.g. sampling is strongly biased towards populated areas
  
  
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
  A presence-absence dataset with an effort-driven sampling approach
  
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
  
  # trim to remove NAs to the east of the study area
  landscape <- terra::trim(landscape)
  
  # extracting the occurrence data
  presence <- terra::unwrap(species[[4]])
  
  # multiply by two to get equal amounts of presence and absence points
  sample_p_2 = sample_p * 2
  
  
  # 3. Preparing the population density data
  #--------------------------------------------------------
  
  # read the population density data
  # from https://www.abs.gov.au/statistics/people/population/regional-population/2023-24#data-downloads
  pop_density <- terra::rast(paste0(envrmt$path_osm, "/pop_density_vic_nsw_act.tif"))
  
  # reproject
  pop_density <- terra::project(pop_density, presence)
  #print("Reprojected...")
  
  # resample to match species grid
  pop_density <- terra::resample(pop_density, presence, method = "bilinear")
  #print("Resampled...")
  
  # mask again just to be sure
  pop_density <- terra::mask(pop_density, presence)
  #print("Masked...")
  
  
  # replace NA and negative values
  pop_density[is.na(pop_density)] <- 0
  pop_density[pop_density < 0] <- 0
  
  # log-scale to reduce cities dominating the data
  #pop_density <- log1p(pop_density)
  
  # normalise to sum = 1 
  pop_density <- pop_density / terra::global(pop_density, "sum", na.rm = TRUE)[1,1]
  #print("Normalised...")
  
  
  # 4. Sampling near the population centers
  #--------------------------------------------------------
  
  # strongly biased sampling towards population centers
  pres_abs_points <- sampleOccurrences(presence, n = sample_p_2, type = "presence-absence",
                                       replacement = FALSE, sample.prevalence = 0.5, 
                                       bias = "manual", bias.strength = 100,
                                       weights=pop_density)
  
  
  # 5. Final formatting of the data
  #--------------------------------------------------------
  
  # convert to dataframe
  pres_abs_df <- as.data.frame(pres_abs_points$sample.points)
  
  # convert to stars vector object
  pres_abs_sf <- sf::st_as_sf(pres_abs_df, coords = c("x", "y"), 
                              crs = terra::crs(landscape),remove = F)
  
  # extracting the data for the presence-points
  species_data_extr <- terra::extract(landscape, pres_abs_sf)
  species_data_compl <- cbind(pres_abs_sf, species_data_extr)
  #print(species_data_compl)
  
  sample_p <- as.character(sample_p)
  
  #print("Extracted the layer data...")
  
  # 6. Saving the data
  #--------------------------------------------------------
  
  # creating directory for the presence absence data
  dir_pres <- paste0(envrmt$path_pre_abs_points, "/EffortDriven/", species_name, "/", sample_p)
  if(!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  # saving the presence absence data
  sf::write_sf(species_data_compl, paste0(dir_pres, "/", species_name, "_Fit_", 
                                          fit, "_Iteration_", iter, "_Pres_Abs.gpkg"))

}
