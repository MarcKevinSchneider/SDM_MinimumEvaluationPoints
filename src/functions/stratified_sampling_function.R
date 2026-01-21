#' @name stratified_sampling_function.R
#' @date 21.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for stratified sampling the presence points and absence points

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
# 2. Stratified Sampling function
# ================================================================

stratified_sampling <- function(species_name, fit, sample_p, iter){
  '
  Purpose: Samples presence-absence points using a stratified sampling approach.
           Uses the five strata for the four layers of the original bioclim dataset.
  
  
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
  A presence-absence dataset with a stratified sampling approach
  
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
  
  
  # 3. Reading the stratified data
  #--------------------------------------------------------
  
  # decides randomly which layer strata should be used
  random_num <- as.numeric(sample(c(1, 3, 7, 12), 1))
  #print(random_num)
  
  # read the strata
  strat_1 <- terra::rast(paste0(envrmt$path_bioclim_strata, "/bio_", random_num ,"_class_1.tif"))
  strat_2 <- terra::rast(paste0(envrmt$path_bioclim_strata, "/bio_", random_num ,"_class_2.tif"))
  strat_3 <- terra::rast(paste0(envrmt$path_bioclim_strata, "/bio_", random_num ,"_class_3.tif"))
  strat_4 <- terra::rast(paste0(envrmt$path_bioclim_strata, "/bio_", random_num ,"_class_4.tif"))
  strat_5 <- terra::rast(paste0(envrmt$path_bioclim_strata, "/bio_", random_num ,"_class_5.tif"))
  
  # put them into a list
  strata <- list(strat_1, strat_2, strat_3, strat_4, strat_5)
  
  
  # 4. Calculate strata size and number of samples per stratum
  #--------------------------------------------------------
  
  # number of valid cells per stratum
  stratum_sizes <- sapply(strata, function(r) {
    terra::global(!is.na(r), "sum", na.rm = TRUE)[1, 1]
  })
  
  # total area proxy
  total_size <- sum(stratum_sizes)
  
  # proportional allocation
  n_per_stratum <- round(sample_p_2 * stratum_sizes / total_size)
  #print(n_per_stratum)
  
  # fix rounding issues 
  diff <- sample_p_2 - sum(n_per_stratum)
  if (diff != 0) {
    n_per_stratum[which.max(stratum_sizes)] <-
      n_per_stratum[which.max(stratum_sizes)] + diff
  }
  
  # avoid impossible 50/50 splits
  n_per_stratum[n_per_stratum < 2] <- 0
  
  
  # 5. Proportionate sampling
  #--------------------------------------------------------
  
  # to vector so that I can use the rasters
  samples <- vector("list", length(strata))
  
  # loop all stratas
  for (i in seq_along(strata)) {
    
    if (n_per_stratum[i] == 0) next
    
    # convert stratum raster to polygon
    strat_poly <- terra::as.polygons(strata[[i]],values = FALSE,na.rm = TRUE)
    
    #print(n_per_stratum[i])
    
    # have to implement a try statement here since the code crashes for some iterations
    res <- try(sampleOccurrences(presence, n = n_per_stratum[i], type = "presence-absence",
                                 replacement = FALSE,sample.prevalence = 0.5,
                                 sampling.area = strat_poly, plot = FALSE),
               silent = TRUE)
    
    # keep only valid results
    if (!inherits(res, "try-error") && !is.null(res$sample.points) && nrow(res$sample.points) > 0) {
      samples[[i]] <- res$sample.points
    }
  }
  
  # remove failed strata
  samples <- samples[!sapply(samples, is.null)]
  
  
  #if (length(samples) == 0) {
  #  stop(paste0("Iteration", iter, ": no valid samples could be drawn."))
  #}
  
  # bind the data for all strata together
  pres_abs_points <- do.call(rbind, samples)
  
  # remove points with NA coordinates
  pres_abs_points <- pres_abs_points[!is.na(pres_abs_points$x) &!is.na(pres_abs_points$y),]
  
  # 5. Final formatting of the data
  #--------------------------------------------------------
  
  # convert to stars vector object
  pres_abs_sf <- sf::st_as_sf(pres_abs_points, coords = c("x", "y"), 
                              crs = terra::crs(landscape), remove = FALSE)
  
  
  # extracting the data for the presence-points
  species_data_extr <- terra::extract(landscape, pres_abs_sf)
  species_data_compl <- cbind(pres_abs_sf, species_data_extr)
  #print(species_data_compl)
  
  sample_p <- as.character(sample_p)
  
  #print("Extracted the layer data...")
  
  # 6. Saving the data
  #--------------------------------------------------------
  
  # creating directory for the presence absence data
  dir_pres <- paste0(envrmt$path_pre_abs_points, "/Stratified/", species_name, "/", sample_p)
  if(!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  # saving the presence absence data
  sf::write_sf(species_data_compl, paste0(dir_pres, "/", species_name, "_Fit_", 
                                          fit, "_Iteration_", iter, "_Pres_Abs.gpkg"))
}
