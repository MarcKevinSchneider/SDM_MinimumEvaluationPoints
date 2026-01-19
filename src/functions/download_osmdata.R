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
# 5. Download the OSM population data
# ================================================================

# build query
place_query <- opq(bbox = bbox,timeout = 180) |>
  add_osm_feature(key = "place",value = c("city", "town", "village"))

# download the data
places_osm <- osmdata_sf(place_query)

# extract population extents, transform, etc.
places_sf <- places_osm$osm_points
places_sf <- sf::st_transform(places_sf, terra::crs(landscape))
places_sf <- sf::st_crop(places_sf, sf::st_bbox(ext_poly))

# new folder for the osm data
osm_dir <- paste0(envrmt$path_data, "/osm")
if (!dir.exists(osm_dir)) dir.create(osm_dir, recursive = TRUE)
sf::write_sf(places_sf,paste0(osm_dir, "/places_osm.gpkg"))

# ================================================================
# 6. Download the OSM state border data
# ================================================================

# build query
state_query <- opq(bbox = bbox, timeout = 180) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "admin_level", value = "4")

# download the data
states_osm <- osmdata_sf(state_query)

# extract border extents, transform, etc.
states_sf <- states_osm$osm_multipolygons
states_sf <- sf::st_transform(states_sf, terra::crs(landscape))
states_sf <- sf::st_crop(states_sf, sf::st_bbox(ext_poly))

# save
sf::write_sf(states_sf,paste0(osm_dir, "/states_osm.gpkg"))

# ================================================================
# 7. Download the OSM county border data
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
# 8. Download the OSM aboriginal lands border data
# ================================================================

# build query
aboriginal_query <- opq(bbox = bbox, timeout = 180) |>
  add_osm_feature(key = "boundary", value = "aboriginal_lands")

# download the data
aboriginal_osm <- osmdata_sf(aboriginal_query)

# extract border extents, transform, etc.
aboriginal_sf <- aboriginal_osm$osm_multipolygons
aboriginal_sf <- sf::st_transform(aboriginal_sf, terra::crs(landscape))
aboriginal_sf <- sf::st_crop(aboriginal_sf, sf::st_bbox(ext_poly))

# save
sf::write_sf(aboriginal_sf,
             paste0(osm_dir, "/aboriginal_areas_osm.gpkg"))

# ================================================================
# 9. Download the OSM national park data
# ================================================================

# build query
#park_query <- opq(bbox = bbox,timeout = 180) |>
#  add_osm_feature(key = "boundary", value = "protected_area") |>
#  add_osm_feature(key = "protect_class",value = "2")

park_query <- opq(bbox = bbox,timeout = 180) |>
  add_osm_feature(key = "boundary",value = "protected_area")

# download the data
parks_osm <- osmdata_sf(park_query)

# extract park extents, transform, etc.
#parks_sf <- parks_osm$osm_multipolygons
parks_sf <- dplyr::bind_rows(parks_osm$osm_multipolygons,parks_osm$osm_polygons)

parks_sf <- sf::st_transform(parks_sf, terra::crs(landscape))
parks_sf <- sf::st_crop(parks_sf, sf::st_bbox(ext_poly))

parks_np <- parks_sf[grepl("national park", parks_sf$name, ignore.case = TRUE),]

parks_np <- parks_np[parks_np$boundary == "protected_area", ]

names(parks_np) <- make.unique(names(parks_np))
parks_np <- sf::st_make_valid(parks_np)
parks_np <- sf::st_collection_extract(parks_np, "POLYGON")

parks_np <- parks_np |>
  dplyr::select(
    name,
    operator,
    boundary,
    protect_class,
    geometry
  )

out_file <- paste0(osm_dir, "/test_australia_national_parks_osm.gpkg")

if (file.exists(out_file)) file.remove(out_file)

sf::st_write(
  parks_np,
  dsn   = out_file,
  layer = "national_parks",
  quiet = FALSE
)

