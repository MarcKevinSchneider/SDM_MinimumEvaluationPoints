#' @name 02_sampling_presence_points.R
#' @date 09.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Samples the 1 to 300 evaluation points for the analysis. Employs the sampling
#' strategies used in this analysis.

# ================================================================
# 1. Load setup script and function script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/Minimum_Evaluation_Points_SDM/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

source(paste0(envrmt$path_src, "/functions/sampling_function.R"))

# ================================================================
# 2. Sampling the presence, absence and background points using the sampling strategies
# ================================================================

random_sampling("VS01", "0.1", 1)
