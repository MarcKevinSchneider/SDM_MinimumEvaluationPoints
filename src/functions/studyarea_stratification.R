#' @name studyarea_stratification.R
#' @date 21.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Splits the study area into five different classes for all four bioclim variables

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
# 2. Stratification of the original study area
# ================================================================

# read the original bioclim data
bioclim <- terra::rast(paste0(envrmt$path_data, "/variables.tif"))

# output directory
out_dir <- paste0(envrmt$path_data, "/bioclim_strata")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# number of classes
n_classes <- 5

# loop over layers
for (i in seq_len(nlyr(bioclim))) {
  
  # each layer individually
  layer <- bioclim[[i]]
  layer_name <- names(layer)
  
  # compute range of each layer
  r_min <- terra::global(layer, "min", na.rm = TRUE)[1, 1]
  r_max <- terra::global(layer, "max", na.rm = TRUE)[1, 1]
  
  # class breaks
  breaks <- seq(r_min, r_max, length.out = n_classes + 1)
  
  # create each class as its own raster
  for (k in seq_len(n_classes)) {
    
    class_rast <- layer
    class_rast[] <- NA
    
    class_rast[layer >= breaks[k] & layer < breaks[k + 1]] <- 1
    
    # include max value in last class
    if (k == n_classes) {
      class_rast[layer == r_max] <- 1
    }
    
    # filename
    out_file <- paste0(out_dir, "/",layer_name, "_class_", k, ".tif")
    
    # save
    terra::writeRaster(class_rast,out_file,overwrite = TRUE)
  }
}
