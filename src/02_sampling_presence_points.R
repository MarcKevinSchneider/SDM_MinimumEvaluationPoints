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
rootDir <- "/home/Marc/SDM_MinimumEvaluationPoints"
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

# full parameters
params <- expand.grid(
  sp   = as.character(c("VS01", "VS02", "VS03", "VS04", "VS05", 
                        "VS06", "VS07", "VS08", "VS09", "VS10")),
  fit  = as.character(c("0.1", "0.2", "0.3", "0.4", "0.5", 
                        "0.6", "0.7", "0.8", "0.9")),
  n    = as.numeric(seq(1, 300, 1)),
  iter = as.numeric(seq(1, 10, 1)),
  stringsAsFactors = FALSE
)

# ================================================================
# 3. Sampling the background points randomly
# ================================================================

# parameters for the 10,000 randomy background points
# dont need iteration and sample size here
bck_params <- expand.grid(
  sp   = as.character(c("VS01", "VS02", "VS03", "VS04", "VS05", 
                        "VS06", "VS07", "VS08", "VS09", "VS10")),
  fit  = as.character(c("0.1", "0.2", "0.3", "0.4", "0.5",
                        "0.6", "0.7", "0.8", "0.9")),
  stringsAsFactors = FALSE
)

# sample 10,000 background points for each species and then one iteration per fit
nCores <- 48

mclapply(
  X = seq_len(nrow(bck_params)),
  FUN = function(i) {
    bck_sampling(bck_params$sp[i], bck_params$fit[i])},
  mc.cores = nCores
)

# ================================================================
# 4. Sampling a balanced amount of background points
# ================================================================

# sample a specified number of background points for each species, each fit and 
# then a few iterations of it

# number of cores for parallelization
nCores <- 48

mclapply(
  X = seq_len(nrow(params)),
  FUN = function(i) {
    balanced_bck_sampling(params$sp[i], params$fit[i], params$n[i], params$iter[i])
  },
  mc.cores = nCores
)

# ================================================================
# 5. Setting up parallelization
# ================================================================

# number of cores for parallelization
nCores <- 48

# sample functions
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

# ================================================================
# 6. Sampling the presence-absence points using the sampling strategies
# ================================================================

# iterate over all functions
failures <- mclapply(
  X = seq_len(nrow(params)),
  FUN = function(i) {
    
    sp <- params$sp[i]
    fit <- params$fit[i]
    n <- params$n[i]
    iter <- params$iter[i]
    
    local_failures <- list()
    
    # tries to sample for each sampling function
    # throws an error if it doesnt work
    for (name in names(samplers)) {
      tryCatch(
        {
          samplers[[name]](sp, fit, n, iter)
          NULL
        },
        error = function(e) {
          local_failures[[length(local_failures) + 1]] <<- data.frame(
            function_name = name,
            species       = sp,
            fit           = fit,
            n             = n,
            iteration     = iter,
            error_message = conditionMessage(e),
            stringsAsFactors = FALSE
          )
        }
      )
    }
    
    if (length(local_failures) == 0) NULL else do.call(rbind, local_failures)
  },
  mc.cores = nCores
)


# ================================================================
# 7. Final checks and cleanup
# ================================================================

# combine the logs from the workers
failed_runs <- do.call(rbind, failures)

# saves if there are failed runs
if (!is.null(failed_runs)) {
  write.csv(failed_runs, paste0(envrmt$path_docs,"/Systematic_sampling_failures_09032026.csv"), 
            row.names = FALSE)
}





