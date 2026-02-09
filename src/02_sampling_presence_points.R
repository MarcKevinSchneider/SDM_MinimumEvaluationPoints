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
                 "snowball", "leaveOut", "stratified", "effortDriven", "preferential")

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
  n    = as.numeric(seq(100, 105, 1)),
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

# loop over all parameters
'lapply(1:nrow(params), function(i){
  random_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  cluster_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i], plot=FALSE)
  block_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  conv_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  systematic_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  snowball_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  leaveout_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  stratified_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  effort_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  preferential_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
})'

# ================================================================
# 5. Setting up parallelization
# ================================================================

# maximum cores without crashing my pc
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
# setting the seed for the cluster
clusterSetRNGStream(cl, 2962)


# exporting for the cluster workers
clusterExport(cl, varlist = c("envrmt","params","random_sampling", "cluster_sampling", "block_sampling",
                              "conv_sampling", "systematic_sampling", "snowball_sampling", 
                              "leaveout_sampling", "stratified_sampling", "effort_sampling", 
                              "preferential_sampling"), envir = environment())

clusterEvalQ(cl, {
  library(envimaR)          # for the folder structure
  library(dplyr)            # data manipulation
  library(sf)               # spatial vector data
  library(parallel)         # parallel processing
  library(RandomFields)     # Gaussian random fields
  library(NLMR)             # neutral landscape models
  library(terra)            # raster handling
  library(climateStability) # rescaling to [0,1]
  library(RandomFieldsUtils)# dependency RandomField package
  library(raster)           # dependency RandomField package
  library(virtualspecies)   # for virtual species
  library(ggplot2)          # for plotting
  library(blockCV)          # for some of the sampling strategies
  library(Metrics)          # for the evaluation metrics 
  library(tidyverse)        # general functions
  library(geodata)          # for the download of the border data
  library(PresenceAbsence)  # for the binary cutoff of the predictions
  library(osmdata)          # for the street and population data
})


# all the sampling functions
samplers <- list(
  random       = random_sampling,
  cluster      = cluster_sampling,
  block        = block_sampling,
  conv         = conv_sampling,
  systematic   = systematic_sampling,
  snowball     = snowball_sampling,
  leaveout     = leaveout_sampling,
  stratified   = stratified_sampling,
  effort       = effort_sampling,
  preferential = preferential_sampling
)

# exporting the list of samplign functions 
clusterExport(cl, "samplers", envir = environment())

# ================================================================
# 6. Sampling the presence-absence points using the sampling strategies
# ================================================================

# parallelized loop for all the functions
failures <- parLapply(cl, seq_len(nrow(params)), function(i) {
  
  sp   <- params$sp[i]
  fit  <- params$fit[i]
  n    <- params$n[i]
  iter <- params$iter[i]
  
  # empty list for the failed runs
  local_failures <- list()
  
  # try catch so that we can log the errors and which runs failed
  for (name in names(samplers)) {
    tryCatch(
      samplers[[name]](sp, fit, n, iter),
      error = function(e) {
        local_failures[[length(local_failures) + 1]] <<- data.frame(
          function_name = name,
          species = sp,
          fit = fit,
          n = n,
          iteration = iter,
          error_message = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }
    )
  }
  
  if (length(local_failures) == 0) NULL else do.call(rbind, local_failures)
})

# ================================================================
# 7. Final checks and cleanup
# ================================================================

# combine the logs from the workers
failed_runs <- do.call(rbind, failures)

# saves if there are failed runs
if (!is.null(failed_runs)) {
  write.csv(failed_runs, paste0(envrmt$path_docs,"/sampling_failures.csv"), 
            row.names = FALSE)
}

# stop the cluster
stopCluster(cl)




