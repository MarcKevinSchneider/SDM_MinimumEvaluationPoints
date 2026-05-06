#' @name map_functions.R
#' @date 05.04.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for generating the overview plots
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

# ================================================================
# 2. WorldClim raster to df
# ================================================================

raster_to_df <- function(r, layer_name) {
  df <- as.data.frame(r[[layer_name]], xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "value")
  df$variable <- layer_name
  df
}

# ================================================================
# 3. Make panel for the Study Area
# ================================================================

make_studyarea_panel <- function(layer_name, palette, units_label, direction = 1) {
  '
  Purpose: Creates a subplot for the overview plot of the study area

  Parameters:
  ----------------------------
  layer_name: str   - Name of layer to be plotted
  palette:    str   - Name of color palette to be used
  units_label: str  - Unit label for the color scale
  direction:  int   - Direction of palette; 1 (normal) or -1 (inverted)

  Returns:
  ---------------------------
  A ggplot subplot of the specified layer
  '
  df <- raster_to_df(bioclim_mask, layer_name)
  
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = study_states, fill = NA, colour = "grey20", linewidth = 0.45) +
    scale_fill_distiller(
      palette   = palette,
      direction = direction,
      name      = units_label,
      na.value  = "transparent",
      breaks    = scales::pretty_breaks(n = 10),
      guide     = guide_colorbar(barwidth = 1, barheight = 7,
                                 title.position = "top", title.hjust = 0.5)
    ) +
    coord_sf(
      xlim   = c(bbox["xmin"], bbox["xmax"] - 300000),
      ylim   = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE,
      datum  = sf::st_crs(4326)
    ) +
    labs(title = layer_name, x = NULL, y = NULL) +
    theme_void(base_size = 9) +
    theme(
      plot.title        = element_text(face = "bold", hjust = 0.5, size = 8, margin = margin(b = 3)),
      legend.position   = "right",
      legend.title      = element_text(size = 7),
      legend.text       = element_text(size = 6),
      plot.margin       = margin(4, 4, 4, 4),
      panel.grid.major  = element_line(colour = "grey70", linewidth = 0.25, linetype = "dashed"),
      axis.text.x       = element_text(size = 6, colour = "grey30"),
      axis.text.y       = element_text(size = 6, colour = "grey30"),
      axis.ticks        = element_line(colour = "grey50", linewidth = 0.2),
      axis.ticks.length = unit(2, "pt")
    ) +
    scale_x_continuous(breaks = seq(140, 154, by = 2)) +
    scale_y_continuous(breaks = seq(-40, -28, by = 2))
}

# ================================================================
# 3. Virtual species raster to df
# ================================================================

vs_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "value")
  df$value <- factor(df$value, levels = c(0, 1))
  df
}

# ================================================================
# 4. Make panel for the Virtual Species
# ================================================================

make_vs_panel <- function(vs_name, prev) {
  '
  Purpose: Creates a subplot for the overview plot of the virtual species
  
  
  Parameters:
  ----------------------------
  
  vs_name: str
    Name of the virtual species
    
  prev: float
    Prevalence of the virtual species as specified by Grimmet et al. 2020
    
  
  Returns:
  ---------------------------
  A subplot of the specified virtual species
  
  '
  df <- vs_to_df(vs_rasters[[vs_name]])
  ggplot() +
    geom_sf(data = study_states, fill = "grey", colour = "grey30", linewidth = 0.3) +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    geom_sf(data = study_states, fill = NA, colour = "grey30", linewidth = 0.3) +
    scale_fill_manual(
      values   = c("0" = "grey", "1" = "darkgreen"),
      labels   = c("0" = "Study region", "1" = "Species distribution"),
      na.value = "transparent", name = NULL, drop = FALSE) +
    coord_sf(
      xlim   = c(bbox["xmin"], bbox["xmax"] - 300000),
      ylim   = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE, datum  = sf::st_crs(4326)) +
    labs(title = sprintf("%s (%.2f)", vs_name, prev)) +
    theme_void(base_size = 8) +
    theme(plot.title = element_text(face = "bold", hjust = 1, size = 12, 
                                    margin = margin(t = 2, b = 1)),
          legend.position = "none", plot.margin = margin(2, 2, 2, 2))
}