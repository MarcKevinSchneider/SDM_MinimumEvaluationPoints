#' @name cluster_sampling_function.R
#' @date 09.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for cluster sampling the presence points and absence points

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

# ================================================================
# 3. Cluster Sampling function
# ================================================================

cluster_sampling <- function(species_name, fit, sample_p, iter, plot = FALSE){
  '
  Purpose: Samples presence-absence points using an environmental clusters approach
  
  
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
    
  plot: bool
    Decides whether the presence-absence points should get plotted or not
    
  
  Returns:
  ---------------------------
  A presence-absence dataset with environmental clusters
  
  '
  
  # 1. Ensuring data structure
  #--------------------------------------------------------
  species_name <- as.character(species_name)
  fit <- as.character(fit)
  sample_p <- as.numeric(sample_p)
  iter <- as.character(iter)
  
  
  # 2. Correctly reading the data
  #--------------------------------------------------------
  
  # reading the species and landscape data
  species <- readRDS(paste0(envrmt$path_VirtualSpecies, "/", species_name, ".rds"))
  landscape <- terra::rast(paste0(envrmt$path_ADM, "/", species_name, "/",
                                  species_name, "_Fit_", fit, ".tif"))
  
  # trim to remove NAs to the east of the study area
  landscape <- terra::trim(landscape)
  
  presence <- terra::unwrap(species[[4]])
  
  
  # 3. Determine number of k 
  #--------------------------------------------------------
  
  # if below 5 simply use the sample size
  # if above 5 use the 5 clusters
  if (sample_p <= 5) {
    k_folds <- sample_p
  } else {
    k_folds <- 5
  }
  
  
  
  # 4. Load precomputed clusters
  #--------------------------------------------------------
  
  # load the cluster tif
  cluster_rast <- terra::rast(paste0(envrmt$path_data,
                                     "/env_clusters/cluster_k_", k_folds, ".tif"))
  
  # ensure alignment
  cluster_rast <- terra::resample(cluster_rast, landscape, method = "near")
  

  # 5. Select in which clusters should be sampled
  #--------------------------------------------------------  
  n_sel <- min(3, k_folds)
  sel_clusters <- sample(1:k_folds, n_sel)
  
  
  # 6. Restrict presence and absence to rasters
  #-------------------------------------------------------- 
  
  # filter for presence and absence cells
  pres_cells <- which(values(presence) == 1)
  abs_cells  <- which(values(presence) == 0)
  
  # filter for clusters
  pres_clusters <- values(cluster_rast)[pres_cells]
  abs_clusters  <- values(cluster_rast)[abs_cells]
  
  # filter for the selected clusters
  pres_cells_sel <- pres_cells[pres_clusters %in% sel_clusters]
  abs_cells_sel  <- abs_cells[abs_clusters %in% sel_clusters]
  

  
  # 7. Sample presence absence points
  #-------------------------------------------------------- 
  
  # sample the presence points
  pres_sample_cells <- sample(pres_cells_sel,
                              size = min(sample_p, length(pres_cells_sel)),replace = FALSE)
  
  # sample the absence points
  abs_sample_cells <- sample(abs_cells_sel,
                             size = min(sample_p, length(abs_cells_sel)),replace = FALSE)
  

  
  # 8. Convert to sf
  #-------------------------------------------------------- 
  
  # reconstruct to grid
  pres_xy <- terra::xyFromCell(presence, pres_sample_cells)
  abs_xy  <- terra::xyFromCell(presence, abs_sample_cells)
  
  # convert to dataframe
  # for presence
  pres_df <- data.frame(x = pres_xy[,1],y = pres_xy[,2],Observed = 1)
  
  # and for absence
  abs_df <- data.frame(x = abs_xy[,1],y = abs_xy[,2],Observed = 0)
  
  # bind both
  pres_abs_df <- rbind(pres_df, abs_df)
  
  
  # to sf 
  pres_abs_sf <- sf::st_as_sf(pres_abs_df,coords = c("x", "y"),
                              crs = terra::crs(landscape),remove = FALSE)
  
  # add cluster IDs
  pres_abs_sf$cluster <- terra::extract(cluster_rast, pres_abs_sf)[,2]
  

  
  # 9. Extract layer values
  #--------------------------------------------------------
  
  species_data_extr <- terra::extract(landscape, pres_abs_sf)
  species_data_compl <- cbind(pres_abs_sf, species_data_extr)
  

  
  # 10. Optional plotting
  #--------------------------------------------------------
  
  if (plot) {
    
    land_df <- as.data.frame(landscape, xy = TRUE)
    colnames(land_df)[3] <- "value"
    
    p <- ggplot() +
      geom_raster(data = land_df,
                  aes(x = x, y = y, fill = value),
                  alpha = 0.4) +
      scale_fill_viridis_c() +
      geom_point(
        data = pres_abs_sf,
        aes(x = x, y = y, color = factor(cluster),
            shape = factor(Observed)),
        size = 2
      ) +
      scale_color_brewer(palette = "Set1") +
      coord_equal() +
      labs(
        title = paste0("Cluster-based blocked sampling: ", species_name),
        subtitle = paste("Clusters:", paste(sel_clusters, collapse = ", ")),
        color = "Cluster",
        shape = "Observed"
      ) +
      theme_minimal()
    
    print(p)
  }
  

  
  # 11. Save output
  #--------------------------------------------------------
  
  sample_p_char <- as.character(sample_p)
  
  dir_pres <- paste0(envrmt$path_pre_abs_points,"/Cluster/", species_name, "/", sample_p_char)
  if (!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  
  sf::write_sf(species_data_compl, paste0(dir_pres, "/", species_name, "_Fit_", fit,
           "_Iteration_", iter, "_Pres_Abs.gpkg"))
}
