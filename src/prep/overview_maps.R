#' @name overview_maps.R
#' @date 05.04.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Script for generating the overview maps for the study area and virtual species.
#' 

# ================================================================
# 1. Load setup script and function script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# set seed
set.seed(2962)

source(paste0(envrmt$path_src, "/prep/map_functions.R"))

# ================================================================
# 2. Study area overview map
# ================================================================

# read the original bioclim data
bioclim <- terra::rast(paste0(envrmt$path_data, "/variables.tif"))

# states of the study area
study_states <- sf::st_read(paste0(envrmt$path_osm, "/states_australia.gpkg"), quiet=TRUE)
bbox <- sf::st_bbox(study_states)

# complete australia states
australia <- sf::st_read(paste0(envrmt$path_osm, "/Complete_Australia_States.gpkg"), quiet=TRUE)
study_bbox_poly <- sf::st_as_sfc(sf::st_bbox(study_states)) |> sf::st_sf(crs = sf::st_crs(study_states))

# crop and mask
bioclim_crop <- terra::crop(bioclim, terra::vect(study_states))
bioclim_mask <- terra::mask(bioclim_crop, terra::vect(study_states))

names(bioclim_mask) <- c("Annual Mean Temperature", "Isothermality",
                         "Annual Temperature Range", "Annual Precipitation")

# overview plot of australia
p_australia <- ggplot() +
  geom_sf(data = australia, fill = "grey85", colour = "grey40", linewidth = 0.3) +
  geom_sf(data = study_states, fill = "grey60", colour = "grey30", linewidth = 0.3) +
  geom_sf(data = study_bbox_poly, fill = NA, colour = "#CC0000", linewidth = 0.6, linetype = "solid") +
  ggspatial::annotation_north_arrow(location = "tr", style = north_arrow_minimal(text_size = 6), 
                                    height = unit(0.7, "cm"), width = unit(0.5, "cm")) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.5, bar_cols = c("grey30", "white"), 
                              line_width = 0.3) +
  labs(title = "Australia") +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 9, margin = margin(b = 3)), 
        panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.4), 
        plot.margin = margin(4, 4, 4, 4))

# reproject centroids to WGS84 before extracting lon/lat
study_states_valid <- sf::st_make_valid(study_states)
centroids_sf <- sf::st_centroid(study_states_valid) |> sf::st_transform(4326)
coords <- sf::st_coordinates(centroids_sf)

# centroids so we can assign the names to the states
study_states_centroids <- data.frame(lon = coords[, 1],lat = coords[, 2], 
                                     STE_NAME21 = study_states_valid$STE_NAME21)

# Reproject both layers to WGS84 for plotting
australia_wgs <- sf::st_transform(australia, 4326)
study_states_wgs <- sf::st_transform(study_states, 4326)

# WGS84 bbox for coord_sf limits
bbox_wgs <- sf::st_bbox(study_states_wgs)


# zoom in of the study area states
p_studyarea <- ggplot() +
  geom_sf(data = australia_wgs, fill = "grey90", colour = "grey60", linewidth = 0.25) +
  geom_sf(data = study_states_wgs, fill = "grey65", colour = "grey20", linewidth = 0.45) +
  geom_text(data = study_states_centroids, aes(x = lon, y = lat + 0.3, label = STE_NAME21), 
            size = 3, colour = "grey10", fontface = "bold", check_overlap = TRUE) +
  coord_sf(xlim  = c(bbox_wgs["xmin"], 154), ylim = c(bbox_wgs["ymin"], bbox_wgs["ymax"]), expand = FALSE) +
  labs(title = "Study Area", x = NULL, y = NULL) +
  theme_void(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 9, margin = margin(b = 3)),
    panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.4),
    plot.margin = margin(4, 4, 4, 4),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.25, linetype = "dashed"),
    axis.text.x = element_text(size = 6, colour = "grey30"),
    axis.text.y = element_text(size = 6, colour = "grey30"),
    axis.ticks = element_line(colour = "grey50", linewidth = 0.2),
    axis.ticks.length = unit(2, "pt")) +
  scale_x_continuous(breaks = seq(140, 154, by = 2)) + scale_y_continuous(breaks = seq(-40, -28, by = 2))



# build the four panels for the plot
p1 <- make_studyarea_panel("Annual Mean Temperature", "RdYlBu", "°C", direction = -1)
p2 <- make_studyarea_panel("Isothermality", "YlOrRd", "%")
p3 <- make_studyarea_panel("Annual Temperature Range","Spectral", "°C", direction = -1)
p4 <- make_studyarea_panel("Annual Precipitation", "RdYlGn", "mm")

# top row has australia and zoomed ins study area
top_row <- p_australia | p_studyarea

# then worldclim subplots
bioclim_grid <- (p1 | p2) / (p3 | p4)

# stack rows vertically
overview_map <- top_row / bioclim_grid + plot_layout(heights = c(1, 2))

# save
ggsave(filename = paste0(envrmt$path_evaluation, "/Plots/Overview_StudyArea_WorldClim.png"), overview_map,
       width = 22, height = 18, units = "cm", dpi = 300)


# ================================================================
# 3. Virtual species overview
# ================================================================

vs_names <- c("VS01", "VS02", "VS03", "VS04", "VS05",
              "VS06", "VS07", "VS08", "VS09", "VS10")

# read the vs tifs
vs_rasters <- lapply(vs_names, function(nm) {
  terra::rast(paste0(envrmt$path_paRaster, "/", nm, ".tif"))
})
names(vs_rasters) <- vs_names

# prevalence of each vs as reported by grimmet 2020
prevalence <- c(0.35, 0.34, 0.33, 0.29, 0.26, 0.21, 0.15, 0.12, 0.11, 0.05)

# buold 10 panels
panels <- lapply(seq_along(vs_names), function(i) {
  make_vs_panel(vs_names[i], prevalence[i])
})

# assemble in 2x5 grid
row1 <- wrap_plots(panels[1:5],  nrow = 1)
row2 <- wrap_plots(panels[6:10], nrow = 1)
vs_overview <- row1 / row2

# save
ggsave(filename = paste0(envrmt$path_evaluation, "/Plots/Overview_VirtualSpecies.png"),
       plot = vs_overview, width = 35, height = 16, units = "cm", dpi = 300)
