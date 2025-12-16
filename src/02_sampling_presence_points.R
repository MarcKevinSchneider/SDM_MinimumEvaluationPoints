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
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

source(paste0(envrmt$path_src, "/functions/sampling_function.R"))

# ================================================================
# 2. Sampling the presence, absence and background points using the sampling strategies
# ================================================================

#params <- expand.grid(
#  sp   = as.character(c("VS01", "VS02", "VS03", "VS04", "VS05", "VS06",
#                        "VS07", "VS08", "VS09", "VS10")),
#  fit  = as.character(c("0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7",
#                        "0.8", "0.9", "1")),
#  n    = as.numeric(seq(1, 300, 1)),
#  iter = as.numeric(seq(1,10,1)),
#  stringsAsFactors = FALSE
#)


# only testing on a small subset of the data right now
params <- expand.grid(
  sp   = as.character(c("VS01", "VS02", "VS03", "VS04", "VS05")),
  fit  = as.character(c("0.1", "0.2", "0.3", "0.4", "0.5")),
  n    = as.numeric(seq(100, 110, 1)),
  iter = as.numeric(seq(1,5,1)),
  stringsAsFactors = FALSE
)

# loop over all parameters
lapply(1:nrow(params), function(i){
  #random_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  cluster_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i], plot=TRUE)
})
