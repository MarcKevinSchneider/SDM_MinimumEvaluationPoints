#' @name leaveOut_sampling_function.R
#' @date 21.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for leave-out-areas sampling the presence points and absence points

# ================================================================
# 1. Load setup script
# ================================================================

rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = FALSE) # echo set to false here to stop the script from printing

# set seed
set.seed(2962)

states <- sf::st_read(paste0(envrmt$path_osm, "/states_australia.gpkg"),
                      quiet=TRUE)

# ================================================================
# 2. Leave-Out-Areas Sampling function
# ================================================================

leaveout_sampling <- function(species_name, fit, sample_p, iter){
  '
  Purpose: Leaves out either counties or states from the sampling process
  
  
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
  A presence-absence dataset with some counties or states left out
  
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
  
  # reading the county and state shapefiles
  # downloaded from OSM and:
  # https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
  counties <- sf::st_read(paste0(envrmt$path_osm, "/counties_nws_vic_act.gpkg"),
                          quiet=TRUE)
  
  states <- sf::st_read(paste0(envrmt$path_osm, "/states_australia.gpkg"),
                        quiet=TRUE)
  
  # 3. randomly choosing states or counties to be left out
  #--------------------------------------------------------
  
  # randomly decide between counties and states
  random_num <- as.numeric(sample(1:2, 1))
  
  #print(random_num)
  
  if (random_num == 1){
    # counties which should be included in the sampling
    # leaves out 100 counties atm
    counties_inc <- counties |> dplyr::slice_sample(n = 158)
    
    pres_abs_points <- sampleOccurrences(presence, n = sample_p_2, type = "presence-absence", 
                                         replacement = FALSE, sample.prevalence = 0.5, 
                                         sampling.area = counties_inc, plot=FALSE)
  } else{
    # randomly sample either NSW or Victoria but always includes ACT
    random_state <- sample(c("NSW", "VIC"), 1)
    
    if (random_state == "NSW") {
      states_inc <- states |>
        dplyr::filter(STE_NAME21 %in% c("New South Wales", "Australian Capital Territory"))
    } else {
      states_state <- states |>
        dplyr::filter(STE_NAME21 %in% c("Victoria", "Australian Capital Territory"))
    }
     # sample only in the two selected states
    pres_abs_points <- sampleOccurrences(
      presence,
      n = sample_p_2,
      type = "presence-absence",
      replacement = FALSE,
      sample.prevalence = 0.5,
      sampling.area = states_inc,
      plot = FALSE
    )
  }
  
  # 4. Final formatting of the data
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
  
  # 5. Saving the data
  #--------------------------------------------------------
  
  # creating directory for the presence absence data
  dir_pres <- paste0(envrmt$path_pre_abs_points, "/LeaveOut/", species_name, "/", sample_p)
  if(!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
  # saving the presence absence data
  sf::write_sf(species_data_compl, paste0(dir_pres, "/", species_name, "_Fit_", 
                                          fit, "_Iteration_", iter, "_Pres_Abs.gpkg"))
}
