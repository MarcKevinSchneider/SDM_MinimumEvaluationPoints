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

test_abs <- sf::read_sf(paste0(envrmt$path_pre_abs_points, 
                    "/Random/VS01/10/VS01_Fit_0.1_Iteration_1_Pres_Abs.gpkg"))

test_abs$Predicted <- ifelse(test_abs$lyr.1 >= 0.5, 1, 0)


# have to do this since we are also extracting the real distribution data now
colnames(test_abs)[colnames(test_abs) == "Observed"] <- "Predicted"

raster_test <- terra::rast(paste0(envrmt$path_paRaster, "/VS01.tif"))

# convert to sf
test_points <- sf::st_as_sf(test_abs, coords = c("x", "y"), crs = terra::crs(raster_test))

test_extract <- terra::extract(raster_test, test_points, method = "bilinear")



test_bck <- sf::read_sf(paste0(envrmt$path_bkg_points, 
                               "/Random/VS01/10/VS01_Fit_0.1_Iteration_1_Background.gpkg"))

colnames(test_abs)[colnames(test_abs) == "Observed"] <- "Predicted"



###################################
###################################

lapply(1:nrow(params), function(i){
  
  # 1. Load presence–absence data
  --------------------------------------------------------
  
  # construct file path for the Presence-Absence data
  file_path <- paste0(envrmt$path_pre_abs_points, "/", params$strat[i], "/",
                      params$sp[i], "/", params$n[i], "/", params$sp[i], "_Fit_", 
                      params$fit[i], "_Iteration_", params$iter[i], "_Pres_Abs.gpkg")
  
  # check if file exists
  if (!file.exists(file_path)) return(NULL)
  pres_abs <- sf::read_sf(file_path)
  
  # 2. Load background data
  --------------------------------------------------------
  
  # construct path for background data
  bck_path <- paste0(envrmt$path_bkg_points, "/", params$strat[i], "/",
                     params$sp[i], "/", params$n[i], "/", params$sp[i], "_Fit_", 
                     params$fit[i], "_Iteration_", params$iter[i], "_Background.gpkg")

  # check if file exists
  if (!file.exists(bck_path)) return(NULL)
  bck_pts <- sf::read_sf(bck_path)
  
  # 3. Load artificial distribution map data
  --------------------------------------------------------
  
  # construct path for the ADM data
  ls_path <- paste0(envrmt$path_ADM, "/", params$sp[i], "/", 
                     params$sp[i], "_Fit_", params$fit[i], ".tif")
  
  # check if file exists
  if (!file.exists(ls_path)) return(NULL)
  adm <- terra::rast(ls_path)
  
  # 4. Load original "true" presence–absence raster data
  --------------------------------------------------------
  
  # construct path for the original Presence-Absence raster
  pa_path <- paste0(envrmt$path_paRaster, "/", params$sp[i], ".tif")
  
  # check if file exists
  if (!file.exists(pa_path)) return(NULL)
  paRaster <- terra::rast(pa_path)
  
  
  # 5. Rest of the code
  --------------------------------------------------------
    
  # extract the sample points from the presence absence rds
  pres_abs$Predicted <- ifelse(pres_abs$lyr.1 >= 0.5, 1, 0)

  # have to do this since we are also extracting the real distribution data now
  #colnames(pa_raw)[colnames(pa_raw) == "Observed"] <- "Predicted"
  
  # convert to sf
  #pa_points <- sf::st_as_sf(pa_raw, coords = c("x", "y"), crs = terra::crs(paRaster))
  
  # extract the true data from the original distribution
  #pa_extract <- terra::extract(paRaster, pa_points, method = "bilinear")
  
  # rename to observed
  #colnames(pa_extract)[2] <- "observed"
  
  # bind both dataframes
  #pa_df <- cbind(pa_extract, observed = pa_raw$Predicted)
  
  # calculate metrics
  metrics <- eval_funcs(pa_df)
  
  data.frame(
    strat = params$strat[i],
    sp    = params$sp[i],
    n     = params$n[i],
    fit   = params$fit[i],
    iter  = params$iter[i],
    
    AUC  = m$AUC,
    MAE  = m$MAE,
    RMSE = m$RMSE,
    TSS  = m$TSS,
    COR  = m$COR,
    JAC  = m$JAC,
    DIS  = m$DIS,
    SOR  = m$SOR
  )
})

results_df <- do.call(rbind, results)