#' @name creating_environmental_clusters.R
#' @date 19.01.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Creates the environmental clusters for the cluster sampling function

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
# 2. Cluster environmentally once and save locally
# ================================================================

# path to bioclim variables
bioclim_path  <- paste0(envrmt$path_data, "/variables.tif")

# new folder for the environmental clusters
cluster_dir   <- paste0(envrmt$path_data, "/env_clusters")
if (!dir.exists(cluster_dir)) dir.create(cluster_dir, recursive = TRUE)

# load bioclim
bioclim <- terra::rast(bioclim_path)

# convert raster to dataframe
bioclim_df <- as.data.frame(bioclim, xy = TRUE, na.rm = TRUE)

# scale environmental variables once
env_scaled <- scale(bioclim_df[, -(1:2)])

# template raster
template <- bioclim[[1]]
values(template) <- NA

# loop over k from 1 to 5 since 5 is max and below 5 is only used when n is below 5
for (k in 1:5) {
  # kmeans clustering
  km <- kmeans(env_scaled, centers = k)
  
  # create cluster raster
  cluster_rast <- template
  
  # assigning the cluster ids to the cells of the raster
  idx <- cellFromXY(cluster_rast, bioclim_df[, c("x", "y")])
  cluster_rast[idx] <- km$cluster
  
  # save locally
  writeRaster(cluster_rast, 
              filename = paste0(cluster_dir, "/cluster_k_", k, ".tif"),overwrite = TRUE)
}