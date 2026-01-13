#' @name download_osmdata.R
#' @date 13.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Downloads the OpenStreetMap data for the study area of the analysis.
#' 


# ================================================================
# 1. Load setup script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# ================================================================
# 2. Get the bounding box from the worldclim data
# ================================================================

bioclim_path <- paste0(envrmt$path_data, "/variables.tif")

# load bioclim
landscape <- terra::rast(bioclim_path)

# get extent polygon directly from raster
ext_poly <- terra::as.polygons(terra::ext(landscape), crs = terra::crs(landscape))

# convert to sf
ext_poly_sf <- sf::st_as_sf(ext_poly)

# transform to WGS84 (required by OSM)
ext_poly_wgs84 <- sf::st_transform(ext_poly_sf, 4326)

# extract bbox
bbox <- sf::st_bbox(ext_poly_wgs84)


# ================================================================
# 3. Download the OSM road data
# ================================================================

road_query <- opq(bbox = bbox,timeout = 180) |>
  add_osm_feature(key = "highway",value = c("motorway", "primary", "secondary"))

roads_osm <- osmdata_sf(road_query)

roads_sf <- roads_osm$osm_lines

roads_sf <- sf::st_transform(roads_sf, terra::crs(landscape))

roads_sf <- sf::st_crop(roads_sf, sf::st_bbox(ext_poly))

# new folder for the osm data
osm_dir   <- paste0(envrmt$path_data, "/osm")
if (!dir.exists(osm_dir)) dir.create(osm_dir, recursive = TRUE)
sf::write_sf(roads_sf,paste0(osm_dir, "/roads_osm.gpkg"))

