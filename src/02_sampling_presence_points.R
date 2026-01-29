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

# set seed
set.seed(2962)

samp_strats <- c("random", "block", "cluster", "convenience", "systematic",
                 "snowball", "leaveOut", "stratified", "effortDriven")

# sourcing the sampling functions
for (strat in samp_strats){
  source(paste0(envrmt$path_src, "/functions/", strat, "_sampling_function.R"))
}
# sourcing the function for creating the background points
source(paste0(envrmt$path_src, "/functions/background_points_sampling.R"))

# ================================================================
# 2. Setting the parameters
# ================================================================

# only testing on a small subset of the data right now
params <- expand.grid(
  sp   = as.character(c("VS01", "VS02", "VS03", "VS04", "VS05")),
  fit  = as.character(c("0.1", "0.2", "0.3", "0.4", "0.5")),
  n    = as.numeric(seq(100, 110, 1)),
  iter = as.numeric(seq(1,5,1)),
  stringsAsFactors = FALSE
)

# ================================================================
# 3. Sampling the background points randomly
# ================================================================

# parameters for the 10,000 randomy background points
# dont need iteration and sample size here
bck_params <- expand.grid(
  sp   = as.character(c("VS01", "VS02", "VS03", "VS04", "VS05")),
  fit  = as.character(c("0.1", "0.2", "0.3", "0.4", "0.5")),
  stringsAsFactors = FALSE
)

# sample 10,000 background points for each species and then one iteration per fit
lapply(1:nrow(bck_params), function(i){
  bck_sampling(bck_params$sp[i], bck_params$fit[i])
})

# ================================================================
# 4. Sampling a balanced amount of background points
# ================================================================

# sample a specified number of background points for each species, each fit and 
# then a few iterations of it
# allows for a balanced presence-absence and background point evaluation
lapply(1:nrow(params), function(i){
  balanced_bck_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
})

# ================================================================
# 5. Sampling the presence-absence points using the sampling strategies
# ================================================================
for (strat in samp_strats){
  source(paste0(envrmt$path_src, "/functions/", strat, "_sampling_function.R"))
}

# loop over all parameters
lapply(1:nrow(params), function(i){
  random_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  cluster_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i], plot=FALSE)
  block_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  conv_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  systematic_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  snowball_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  leaveout_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  stratified_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  effort_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
})
