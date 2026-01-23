#' @name download_osmdata.R
#' @date 13.01.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Downloads the OpenStreetMap data for the study area of the analysis.
#' Also used for transforming the geospatial data from other sources.
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

# build query
road_query <- opq(bbox = bbox,timeout = 180) |>
  add_osm_feature(key = "highway",value = c("motorway", "primary", "secondary"))

# download the data
roads_osm <- osmdata_sf(road_query)

# extract roads, transform, etc.
roads_sf <- roads_osm$osm_lines
roads_sf <- sf::st_transform(roads_sf, terra::crs(landscape))
roads_sf <- sf::st_crop(roads_sf, sf::st_bbox(ext_poly))

# new folder for the osm data
osm_dir   <- paste0(envrmt$path_data, "/osm")
if (!dir.exists(osm_dir)) dir.create(osm_dir, recursive = TRUE)
sf::write_sf(roads_sf,paste0(osm_dir, "/roads_osm.gpkg"))


# ================================================================
# 4. Transform to a sampling mask
# ================================================================

# read the road data
roads <- sf::read_sf(paste0(envrmt$path_osm, "/roads_osm.gpkg"))

# read the ADM data for the mask
landscape <- terra::rast(paste0(envrmt$path_ADM, "/VS01/VS01_Fit_0.1.tif"))

# buffer all roads by 1000 m
road_buffer <- sf::st_buffer(roads, dist = 1000)

# convert buffer to terra vector
road_buffer_vect <- vect(road_buffer)

# create empty raster with same extent/res as landscape
road_mask <- rast(landscape)
values(road_mask) <- NA

# rasterize buffer: 1 = allowed sampling area, NA = excluded
road_mask <- rasterize(road_buffer_vect, road_mask, field = 1)

# write as raster
terra::writeRaster(road_mask, filename = paste0(envrmt$path_osm, "/road_sampling_mask.tif"), 
                   overwrite = TRUE)


# ================================================================
# 5. Download the OSM county border data
# ================================================================

# build query
county_query <- opq(bbox = bbox, timeout = 180) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "admin_level", value = c("6","8"))

# download the data
counties_osm <- osmdata_sf(county_query)

# extract border extents, transform, etc.
counties_sf <- counties_osm$osm_multipolygons
counties_sf <- sf::st_transform(counties_sf, terra::crs(landscape))
counties_sf <- sf::st_crop(counties_sf, sf::st_bbox(ext_poly))

# save
sf::write_sf(counties_sf,
             paste0(osm_dir, "/counties_osm.gpkg"))


# ================================================================
# 6. Transform the National Park data
# ================================================================

# Victoria national park data
# from https://discover.data.vic.gov.au/dataset/parks-and-conservation-reserves-parkres
vic <- sf::st_read(paste0(envrmt$path_osm, "/PARKRES.shp"), quiet=TRUE)

# Australian capital territory national park data
# from https://actmapi-actgov.opendata.arcgis.com/datasets/82a13a6bba6c4bce91e78a495fb754aa_0/explore?location=0.017175%2C0.000000%2C1.83
act <- sf::st_read(paste0(envrmt$path_osm, "/ACTGOV_TP_OVERLAY_ZONE_POLY.shp"), quiet=TRUE)


# NSW national park data
# from https://datasets.seed.nsw.gov.au/dataset/9bad468a-c2a6-4c90-bfaa-8ae67cbb50ef2/resource/33fb1ee3-9090-4cae-a4e7-c4e3c83ba5c8/download/tenure_npws_estateinternalboundaries.zip
nsw <- sf::st_read(paste0(envrmt$path_osm, "/NPWS_EstateInternalBoundaries.shp"),
                   quiet=TRUE)

# check the names of the national parks
unique(vic$AREA_TYPE)
unique(act$OVERLAY_PR)
unique(nsw$TYPE)

# initial filtering to just the national parks
vic_np <- vic |>
  filter(AREA_TYPE == "NATIONAL PARK - SCHEDULE 2, NATIONAL PARKS ACT")

act_np <- act |>
  filter(OVERLAY_PR == "National Park")

nsw_np <- nsw |>
  filter(TYPE == "NATIONAL PARK")


# filter and clean up 
vic_np_clean <- vic_np |>
  dplyr::select(name = NAME, type = AREA_TYPE, geometry = geometry) |>
  dplyr::mutate(state = "Victoria")

# same for NSW
nsw_np_clean <- nsw_np |>
  dplyr::select(name = NAME, type = TYPE, geometry = geometry) |>
  dplyr::mutate(state = "NSW")

# same for NSW
act_np_clean <- act_np |>
  dplyr::select(name = DISTRICT_N, type = OVERLAY_PR, geometry = geometry) |>
  dplyr::mutate(state = "ACT")

# get the crs from the vic dataset
target_crs <- sf::st_crs(vic_np_clean)


# transfrom to vic crs
if(sf::st_crs(nsw_np_clean) != target_crs) {
  nsw_np_clean <- sf::st_transform(nsw_np_clean, target_crs)
}

# transfrom to vic crs
if(sf::st_crs(act_np_clean) != target_crs) {
  act_np_clean <- sf::st_transform(act_np_clean, target_crs)
}

# bind the three datasets
combined_np <- dplyr::bind_rows(vic_np_clean, nsw_np_clean, act_np_clean)

# save as geopackage
sf::st_write(combined_np, paste0(envrmt$path_osm, "/nationalparks_vic_nsw_act.gpkg"))


