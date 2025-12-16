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

rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = FALSE) # echo set to false here to stop the script from printing

# set seed
set.seed(2962)

# ================================================================
# 2. Sampling functions
# ================================================================


# 1 - Random Sampling ####
#-----------------------------------------#

random_sampling <- function(species_name, fit, sample_p, iter){
  '
  Purpose: Samples presence-absence points randomly
  
  
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
  A clustered presence-absence dataset and a background dataset
  
  '
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
  
  
  # sampling the 10,000 bkg points
  background_points <- sf::st_as_sf(as.data.frame(predicts::backgroundSample(mask=landscape, n=10000)), 
                                    crs=terra::crs(landscape), coords=c("x","y"), remove=F)
  
  # extracting the data for the background points
  bg_extr <- terra::extract(landscape, background_points)
  background_points <- cbind(background_points,bg_extr);rm(bg_extr)
  
  # extracting the data for the presence-points
  species_data_extr <- terra::extract(landscape, pres_abs_sf)
  species_data_compl <- cbind(pres_abs_sf, species_data_extr)
  #print(species_data_compl)
  
  sample_p <- as.character(sample_p)
  
  # creating directory for the presence absence data
  dir_pres <- paste0(envrmt$path_pre_abs_points, "/Random/", species_name, "/", sample_p)
  if(!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  # saving the presence absence data
  sf::write_sf(species_data_compl, paste0(dir_pres, "/", species_name, "_Fit_", 
                                          fit, "_Iteration_", iter, "_Pres_Abs.gpkg"))
  # creating directory for the background data
  dir_bkg <- paste0(envrmt$path_bkg_points, "/Random/", species_name, "/", sample_p)
  if(!dir.exists(dir_bkg)) dir.create(dir_bkg, recursive = TRUE)
  # saving the background data
  sf::write_sf(background_points, paste0(dir_bkg, "/", species_name, "_Fit_",
                                         fit, "_Iteration_", iter, "_Background.gpkg"))
  #print(paste0("Saved species data for n=", sample_p, "!"))
}


# 2 - Clustered Sampling Function ####
#-----------------------------------------#

cluster_sampling <- function(species_name, fit, sample_p, iter, plot=FALSE){
  '
  Purpose: Samples based on k-fold clustering
  
  Work-In-Progress!!!
  
  Issue: Calculates the environmental clusters based on the bioclim variables
         but then samples randomly within each cluster
  
  As a result the data is then randomly distributed again; probably need to change
  it so that the points are then sampled close to the cluster center or smth
  
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
    Whether or not to plot where the presence and absence points are
    
  
  Returns:
  ---------------------------
  A clustered presence-absence dataset and a background dataset
  
  '
  
  fit <- as.character(fit)
  
  # load species and landscape data
  species <- readRDS(paste0(envrmt$path_VirtualSpecies, "/", species_name, ".rds"))
  landscape <- terra::rast(paste0(envrmt$path_ADM, "/", species_name, "/", 
                                  species_name, "_Fit_", fit, ".tif"))
  
  # load bioclim variables for clustering
  bioclim <- terra::rast(paste0(envrmt$path_data, "/", "variables.tif"))
  
  # extracting the occurrence data
  presence <- terra::unwrap(species[[4]])
  
  # choose number of clusters (folds)
  # if below sample size < 5 just use the number of sample points as folds
  if (sample_p <= 5) {
    k_folds <- sample_p
    # if sample size is above 5 just use k = 5
  } else {
    k_folds <- 5
  }
  
  # create environmental clusters for all pixels
  #print("Clustering environmental space...")
  
  # convert bioclim raster to data frame of cell centers
  bioclim_df <- as.data.frame(bioclim, xy = TRUE, na.rm = TRUE)
  
  # run k-means clustering on environmental variables
  # based on the amount of k folds selected 
  km <- kmeans(scale(bioclim_df[, -(1:2)]), centers = k_folds)
  
  # extract clusters and add to the bioclim dataframe
  bioclim_df$cluster <- km$cluster
  
  # convert back to raster with cluster IDs
  cluster_rast <- terra::rast(bioclim[[1]])
  values(cluster_rast) <- NA
  
  # map IDs to raster cells
  idx <- cellFromXY(cluster_rast, bioclim_df[, c("x","y")])
  cluster_rast[idx] <- bioclim_df$cluster
  
  # find all presence cells
  presence_cells <- which(values(presence) == 1)
  
  # extract x/y for those cells
  coords <- terra::xyFromCell(presence, presence_cells)
  
  # build presence dataframe
  pres_coords <- data.frame(
    x = coords[,1],
    y = coords[,2],
    presence = 1
  )
  
  # convert the presence points to a sf object
  pres_coords_sf <- sf::st_as_sf(pres_coords,
                                 coords = c("x","y"),
                                 crs = terra::crs(landscape),
                                 remove = FALSE)
  
  # assing each presence point to an environmental cluster
  pres_clusters <- terra::extract(cluster_rast, pres_coords_sf)[,2]
  pres_coords_sf$cluster <- pres_clusters
  

  print("Sampling presence-absence within clusters...")
  
  # presence sample size per cluster
  pres_per_cluster <- ceiling(sample_p / k_folds)
  
  # sample within the clusters
  #pres_sample <- do.call(rbind, lapply(1:k_folds, function(cl){
    # subset presence points belonging to this cluster
  #  pts <- pres_coords_sf[pres_coords_sf$cluster == cl, ]
  #  if (nrow(pts) == 0) return(NULL)
    # sample the requested number of points (or all if fewer exist)
  #  pts[sample(seq_len(nrow(pts)), 
  #             size = min(pres_per_cluster, nrow(pts)), 
  #             replace = FALSE), ]
  #}))
  
  # i actually dont know if this works properly???
  # this is supposed to sample within clusters, weighted toward the centroid
  # of each cluster to have a biased distribution
  pres_sample <- do.call(rbind, lapply(1:k_folds, function(cl){
    
    # subset presence points belonging to this cluster
    pts <- pres_coords_sf[pres_coords_sf$cluster == cl, ]
    if (nrow(pts) == 0) return(NULL)
    
    # extract xy coords
    xy <- sf::st_coordinates(pts)
    
    # compute centroid for this cluster
    centroid <- colMeans(xy)
    
    # inverse distance of each point to the centroid
    d <- sqrt((xy[,1] - centroid[1])^2 + (xy[,2] - centroid[2])^2)
    
    # convert distances into sampling probabilities (closer = higher prob)
    prob <- 1 / (d + 1e-6)  # avoid division by zero
    prob <- prob / sum(prob)  # normalize
    
    # sample using weighted probabilities
    pts[sample(seq_len(nrow(pts)),
               size = min(pres_per_cluster, nrow(pts)),
               replace = FALSE,
               prob = prob), ]
  }))
  
  # now absence sampling inside clusters
  absence_cells <- which(terra::values(presence) == 0)
  
  # also extract x/y for absence cells
  abs_xy <- terra::xyFromCell(presence, absence_cells)
  
  # same structure as for the presence data
  abs_coords <- data.frame(
    x = abs_xy[,1],
    y = abs_xy[,2],
    presence = 0
  )
  
  # absence points to sf
  abs_coords_sf <- sf::st_as_sf(abs_coords,
                                coords=c("x","y"),
                                crs=terra::crs(landscape),
                                remove=FALSE)
  
  # assign clusters to absence points
  abs_clusters <- terra::extract(cluster_rast, abs_coords_sf)[,2]
  abs_coords_sf$cluster <- abs_clusters
  
  # sample absence points within each cluster
  #abs_sample <- do.call(rbind, lapply(1:k_folds, function(cl){
    # subset absence points belonging to this cluster
    #pts <- abs_coords_sf[abs_coords_sf$cluster == cl, ]
    #if (nrow(pts) == 0) return(NULL)
    # sample the requested number of points (or all if fewer exist)
    #pts[sample(seq_len(nrow(pts)), 
    #           size = min(pres_per_cluster, nrow(pts)), 
   #            replace = FALSE), ]
  #}))
  
  abs_sample <- do.call(rbind, lapply(1:k_folds, function(cl){
    
    # subset presence points belonging to this cluster
    pts <- abs_coords_sf[abs_coords_sf$cluster == cl, ]
    
    print(sum(is.na(sf::st_coordinates(pts))))
    print(sum(is.na(pts$cluster)))
    
    if (nrow(pts) == 0) return(NULL)
    
    # extract xy coords
    xy <- sf::st_coordinates(pts)
    
    # compute centroid for this cluster
    centroid <- colMeans(xy)
    
    # inverse distance of each point to the centroid
    d <- sqrt((xy[,1] - centroid[1])^2 + (xy[,2] - centroid[2])^2)
    
    # convert distances into sampling probabilities (closer = higher prob)
    prob <- 1 / (d + 1e-6)  # avoid division by zero
    prob <- prob / sum(prob)  # normalize
    
    # sample using weighted probabilities
    pts[sample(seq_len(nrow(pts)),
               size = min(pres_per_cluster, nrow(pts)),
               replace = FALSE,
               prob = prob), ]
  }))
  
  # set observed to 1 or 0 depending on if the data is presence
  # or absence
  pres_sample$Observed <- 1
  abs_sample$Observed <- 0
  
  # bind both dataframes
  pres_abs_sf <- rbind(pres_sample, abs_sample)
  
  # extract environmental values 
  species_data_extr <- terra::extract(landscape, pres_abs_sf)
  species_data_compl <- cbind(pres_abs_sf, species_data_extr)
  
  # if plot is set to true then this will plot the presence and absence samples
  # for all clusters showing which point belongs to which cluster
  if (plot == TRUE){
    # convert a light-resolution landscape raster to df so the plot isn't gigantic
    land_df <- as.data.frame(landscape, xy = TRUE)
    colnames(land_df)[3] <- "value"
    
    # presence vs absence as a data.frame for ggplot
    pa_df <- data.frame(
      x = pres_abs_sf$x,
      y = pres_abs_sf$y,
      Observed = pres_abs_sf$Observed
    )
    
    # plot
    p <- ggplot() +
      geom_raster(data = land_df, aes(x = x, y = y, fill = value), alpha = 0.4) +
      scale_fill_viridis_c(option = "C") +
      # all presence samples
      geom_point(
        data = pa_df[pa_df$Observed == 1, ],
        aes(x = x, y = y),
        color = "red",
        size = 2
      ) +
      # all absence samples
      geom_point(
        data = pa_df[pa_df$Observed == 0, ],
        aes(x = x, y = y),
        color = "blue",
        size = 2,
        alpha = 0.6
      ) +
      labs(
        title = paste0("Cluster-based Sampling: ", species_name,
                       " | Fit=", fit, " | Iteration=", iter),
        subtitle = "Red = Presence samples | Blue = Absence samples"
      ) +
      coord_equal() +
      theme_minimal()
    
    pa_df$cluster <- pres_abs_sf$cluster
    
    # colors based on the cluster
    p <- p + 
      geom_point(
        data = pa_df,
        aes(x = x, y = y, color = factor(cluster)),
        size = 2
      ) +
      scale_color_brewer(palette = "Set1")
    
    print(p)
  } else {
    NULL
  }
  
  # extract the 10,000 background points
  background_points <- sf::st_as_sf(
    as.data.frame(predicts::backgroundSample(mask = landscape, n = 10000)),
    crs = terra::crs(landscape), 
    coords = c("x","y"), 
    remove = FALSE
  )
  
  bg_extr <- terra::extract(landscape, background_points)
  background_points <- cbind(background_points, bg_extr)
  

  sample_p_char <- as.character(sample_p)
  
  # directories
  dir_pres <- paste0(envrmt$path_pre_abs_points, "/Cluster/", species_name, "/", sample_p_char)
  dir_bkg  <- paste0(envrmt$path_bkg_points, "/Cluster/", species_name, "/", sample_p_char)
  
  if (!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  if (!dir.exists(dir_bkg)) dir.create(dir_bkg, recursive = TRUE)
  
  # save presence absence data
  sf::write_sf(species_data_compl, paste0(
    dir_pres, "/", species_name, "_Fit_", fit, "_Iteration_", iter, "_Pres_Abs.gpkg"))
  # save background data
  sf::write_sf(background_points, paste0(
    dir_bkg, "/", species_name, "_Fit_", fit, "_Iteration_", iter, "_Background.gpkg"))
  
  print(paste0("Cluster-based sampling saved for ", species_name, " (n = ", sample_p, ")"))
}



