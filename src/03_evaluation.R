#' @name 03_evaluation.R
#' @date 16.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Evaluation of each combination using AUC, TSS, RMSE, MAE, Pearson's Correlation,
#' Jaccard's Similarity Index and Sorensen's Similarity Index

# ================================================================
# 1. Load setup script and function script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# sourcing the evaluation functions
source(paste0(envrmt$path_src, "/functions/evaluation_functions.R"))

# ================================================================
# 2. Evaluation
# ================================================================

# parameters for the presence-absence points
params <- expand.grid(
  strat= as.character(c("Random")),
  sp   = as.character(c("VS01")),
  fit  = as.character(c("0.1", "0.2")),
  n    = as.character(seq(1, 10, 1)),
  iter = as.character(seq(1,5,1)),
  stringsAsFactors = FALSE
)

lapply(1:nrow(params), function(i){
  
  
  # construct file path for the Presence-Absence data
  file_path <- paste0(envrmt$path_pre_abs_points, "/", params$strat[i], "/",
                      params$sp[i], "/", params$n[i], "/", params$sp[i], "_Fit_", 
                      params$fit[i], "_Iteration_", params$iter[i], "_Pres_Abs.gpkg")
  
  # check if file exists
  if (!file.exists(file_path)){
    print("Presence-Absence data not found!")
  } else {
    # if file exists then read the file
    pres_abs <- sf::read_sf(file_path)
    #print("File successfully read!")
  }
  
  # construct path for background data
  bck_path <- paste0(envrmt$path_bkg_points, "/", params$strat[i], "/",
                     params$sp[i], "/", params$n[i], "/", params$sp[i], "_Fit_", 
                     params$fit[i], "_Iteration_", params$iter[i], "_Background.gpkg")

  # check if file exists
  if (!file.exists(bck_path)){
    print("Background data not found!")
  } else {
    # if file exists then read the file
    bck_pts <- sf::read_sf(bck_path)
    #print("File successfully read!")
  }
  
  # construct path for the ADM data
  ls_path <- paste0(envrmt$path_ADM, "/", params$sp[i], "/", 
                     params$sp[i], "_Fit_", params$fit[i], ".tif")
  
  # check if file exists
  if (!file.exists(ls_path)){
    print("ADM data not found!")
  } else {
    # if file exists then read the file
    adm <- terra::rast(ls_path)
    #print("File successfully read!")
  }
  
  # construct path for the original Presence-Absence raster
  pa_path <- paste0(envrmt$path_paRaster, "/", params$sp[i], ".tif")
  
  # check if file exists
  if (!file.exists(pa_path)){
    print("Original distribution data not found!")
  } else {
    # if file exists then read the file
    paRaster <- terra::rast(pa_path)
    #print("File successfully read!")
  }
  
  # extract the sample points from the presence absence rds
  pa_raw <- pres_abs[[3]]$sample.points
  # rename from observed to predicted
  # have to do this since we are also extracting the real distribution data now
  colnames(pa_raw)[colnames(pa_raw) == "Observed"] <- "Predicted"
  
  # convert to sf
  pa_points <- sf::st_as_sf(pa_raw, coords = c("x", "y"), crs = terra::crs(paRaster))
  
  # extract the true data from the original distribution
  pa_extract <- terra::extract(paRaster, pa_points, method = "bilinear")
  
  # rename to observed
  colnames(pa_extract)[2] <- "observed"
  
  # bind both dataframes
  pa_df <- cbind(pa_extract, observed = pa_raw$Predicted)
  
  # calculate metrics
  metrics <- eval_funcs(pa_df)


  
  
  
})

