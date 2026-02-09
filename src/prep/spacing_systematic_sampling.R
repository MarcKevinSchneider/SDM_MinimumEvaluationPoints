#' @name spacing_systematic_sampling.R
#' @date 19.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Creates the polygons for the systematic sampling approaches. Grid cell size is based
#' on the extent of the presence pixels of each virtual species for sample sizes 1-300.

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
# 2. Preparing the systematic sampling mask
# ================================================================

# landscape raster for size etc.
landscape <- terra::rast(paste0(envrmt$path_ADM, "/VS01/VS01_Fit_0.1.tif"))

# species names
species_names <- c("VS01", "VS02", "VS03", "VS04", "VS05",
                   "VS06", "VS07", "VS08", "VS09", "VS10")

#species_names <- c("VS09", "VS10")

# loop through virtual species
for (vs in species_names) {
  
  species  <- readRDS(paste0(envrmt$path_VirtualSpecies, "/", vs, ".rds"))
  presence <- terra::unwrap(species[[4]])
  
  # presence-only mask (used for spacing)
  presence_mask <- !is.na(presence[[1]]) & presence[[1]] == 1
  presence_mask <- terra::ifel(presence_mask, 1, NA)
  
  # full valid study area
  study_mask <- !is.na(presence[[1]])
  study_mask <- terra::ifel(study_mask, 1, NA)
  
  
  # polygon for spacing calculation
  presence_poly <- terra::as.polygons(presence_mask, dissolve = TRUE)
  presence_sf   <- sf::st_as_sf(presence_poly)
  presence_sf   <- sf::st_set_crs(presence_sf, sf::st_crs(presence))
  
  # polygon for grid placement
  study_poly <- terra::as.polygons(study_mask, dissolve = TRUE)
  study_sf   <- sf::st_as_sf(study_poly)
  study_sf   <- sf::st_set_crs(study_sf, sf::st_crs(presence))
  
  # area based ONLY on presence cells
  study_area_m2 <- terra::expanse(presence_mask, unit = "m")
  cell_size     <- mean(terra::res(presence))
  
  # output directory
  dir_grid <- paste0(envrmt$path_systematic_grids, "/", vs, "/")
  if (!dir.exists(dir_grid)) dir.create(dir_grid, recursive = TRUE)
  

  # loop over the sampel sizes
  for (n in 1:300) {
    # for presence and absence
    n_total <- n * 2
    
    # initial spacing guess from presence area
    spacing_m <- max(sqrt(study_area_m2 / n_total), cell_size)
    
    # iteratively reduce spacing until enough valid grid cells
    repeat {
      
      # grid from the presence area is expanded over the whole study area
      grid_pts <- sf::st_make_grid(study_sf, cellsize = spacing_m) |>
        sf::st_as_sf() |>
        sf::st_set_crs(sf::st_crs(landscape))
      
      # evaluate grid cells using centroids
      grid_centers <- sf::st_centroid(grid_pts)
      vals <- terra::extract(landscape, terra::vect(grid_centers))
      
      valid_grid <- grid_pts |>
        dplyr::mutate(raster_val = vals[[2]]) |>
        dplyr::filter(!is.na(raster_val)) |>
        dplyr::select(-raster_val)
      
      if (nrow(valid_grid) >= n_total) break
      
      # shrink spacing if not enough cells
      spacing_m <- spacing_m * 0.99
    }
    
    # subsample if too many grid cells
    # uncommented for now since it removes too many cells
    #if (nrow(valid_grid) > n_total) {
    #  valid_grid <- valid_grid[sample(nrow(valid_grid), n_total), ]
    #}
    
    # metadata for the gpkg file
    valid_grid$n_presence <- n
    valid_grid$n_total    <- n_total
    valid_grid$spacing_m  <- spacing_m
    
    # tag presence and absence so that I can check later
    pres_vals <- terra::extract(presence, terra::vect(sf::st_centroid(valid_grid)))
    valid_grid$presence <- pres_vals[[2]] == 1
    

    # save
    sf::write_sf(valid_grid, paste0(dir_grid, "/", vs, "_systematic_grid_n_", n, ".gpkg"), 
                 delete_dsn = TRUE)
  }
}


