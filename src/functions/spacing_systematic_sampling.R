#' @name spacing_systematic_sampling.R
#' @date 19.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Creates the polygon for the systematic sampling approach

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

# read the ADM data for the mask
landscape <- terra::rast(paste0(envrmt$path_ADM, "/VS01/VS01_Fit_0.1.tif"))

# resolution of the raster in x and y
res_land <- terra::res(landscape)   
# mean
cell_size <- mean(res_land)         

# 1000km spacing between points
desired_spacing_m <- 100000000  

# every 122948th pixel with this spacing and resolution
step <- round(desired_spacing_m / cell_size)  

# make a copy of the landscape as a template raster
syst_rast <- landscape[[1]]      # single layer template
values(syst_rast) <- 0           # initialize with 0

# get cell indices of all pixels
all_cells <- 1:ncell(syst_rast)

# select every nth cell
systematic_cells <- all_cells[seq(1, length(all_cells), by = step)]

# set those cells to 1 (allowed sampling locations)
syst_rast[systematic_cells] <- 1

# save
terra::writeRaster(
  syst_rast,
  filename = paste0(envrmt$path_ADM, "/systematic_mask_step_", step, ".tif"),
  overwrite = TRUE
)

# ================================================================
# 3. Sampling mask to sf
# ================================================================

# read the systematic sampling mask with a 200km spacing
syst_rast <- terra::rast(paste0(envrmt$path_ADM, "/systematic_mask_step_246.tif"))

# set 0 to NA
syst_rast[syst_rast == 0] <- NA

# convert all cells with value = 1 into polygons
syst_poly_terra <- terra::as.polygons(syst_rast, values = TRUE, na.rm = TRUE, dissolve = FALSE)

# drop attribute column
syst_poly_terra$bio_1 <- NULL

# terra to sf
syst_poly_sf <- sf::st_as_sf(syst_poly_terra)

# save
sf::write_sf(syst_poly_sf, paste0(envrmt$path_ADM, "/systematic_mask_step_246.gpkg"))

