#' @name get_breakpoint_function.R
#' @date 27.04.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for finding the breakpoint of the evaluation metrics
#' using an exponential decay model. 
#' 


# ================================================================
# 1. Load setup script
# ================================================================

rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = FALSE) # echo set to false here to stop the script from printing

# set seed
set.seed(2962)

# ================================================================
# 2. Curve fitting function
# ================================================================

# helper function for the curve fitting
fit_curve <- function(y_col, n_values){
  '
  Purpose: Fit a non-least squares exponential decay model to the data
  
  
  Parameters:
  ----------------------------
  
  y_col: vector
    Metric column of the df that should be fitted
    
  n_values: vector
    n-values column of the df
    
  
  Returns:
  ---------------------------
  if successful: exponential decay/growth model
  if not successful: error message
  
  '
  y_start <- y_col[1]
  y_end   <- y_col[length(y_col)]
  
  tryCatch({
    # non lest squares exponential model
    mod <- nls(y ~ a * exp(b * n) + c,
               data = data.frame(n = n_values, y = y_col),
               start = list(a = y_start - y_end, b = -0.05, c = y_end),
               control = nls.control(maxiter = 500, tol = 1e-4))
    
    # get coefficients of curve
    cf <- coef(mod)
    n_seq <- seq(min(n_values), max(n_values), by = 1)
    
    list(n_seq = n_seq, slope_seq = abs(cf["a"] * cf["b"] * exp(cf["b"] * n_seq)))
    
  }, error = function(e) {
    message("nls failed: ", e$message)
    NULL
  })
}

# ================================================================
# 3. Breakpoint function
# ================================================================

get_breakpoint <- function(n_values, metric_values, slope_pct = 0.10,
                             weights = c(med = 0.5, range = 0.5)){
  '
  Purpose: Identify the minimum validation size threshold in the data
  
  
  Parameters:
  ----------------------------
  
  n_values: vector/column
    N values of the dataframe
    
  metric_values: vector/column
    Evaluation metric values of the dataframe
    
  slope_pct: float
    Percentage of values at which the breakpoint should be set
    
  
  Returns:
  ---------------------------
  Breakpoint of the data and a summary of the columns
  
  '
  
  # 1. Aggregate to median and range per n
  #--------------------------------------------------------
  summary_data <- data.frame(n = n_values, metric = metric_values) %>%
    dplyr::mutate(n = as.numeric(as.character(n))) %>%
    dplyr::group_by(n) %>%
    # median of the metric 
    dplyr::summarise(med_val = median(metric, na.rm = TRUE), 
                     # range between max and min value
                     range_val = max(metric, na.rm = TRUE) - min(metric, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(med_val)) %>%
    dplyr::arrange(n)
  
  
  # 2. Compute curve for median and value range
  #--------------------------------------------------------
  
  fit_med <- fit_curve(summary_data$med_val, summary_data$n)
  fit_range <- fit_curve(summary_data$range_val, summary_data$n)
  
  
  # 3. Find breakpount from both curves
  #--------------------------------------------------------
  bp <- tryCatch({
    
    # stop if no curve available
    if (is.null(fit_med) && is.null(fit_range)) stop("both curves failed")
    
    n_seq <- if (!is.null(fit_med)) fit_med$n_seq else fit_range$n_seq
    
    # normalize each slope sequence to 0-1 before averaging
    # so both contribute equally regardless of scale
    norm_slope <- function(s) (s - min(s)) / (max(s) - min(s))
    
    slope_med <- if (!is.null(fit_med)) norm_slope(fit_med$slope_seq) else NULL
    slope_range <- if (!is.null(fit_range)) norm_slope(fit_range$slope_seq) else NULL
    
    # calc weights
    w <- weights / sum(weights)
    
    # average whichever curves succeeded
    composite_slope <- dplyr::coalesce(
      # average of both vals if both curves suceeded
      #if (!is.null(slope_med) && !is.null(slope_range)) (slope_med + slope_range) / 2
      if (!is.null(slope_med) && !is.null(slope_range)) {
        # assign weights
        slope_med * w["med"] + slope_range * w["range"]
      }
      # use one of the two curves if one fails
      else if (!is.null(slope_med)) slope_med
      else slope_range
    )
    
    # 4. Find first n where composite slope drops below threshold
    #--------------------------------------------------------
    threshold <- slope_pct * composite_slope[1]
    bp_idx <- which(composite_slope <= threshold)[1]
    n_bp <- n_seq[bp_idx]
    
    n_bp
    
  }, error = function(e) {
    message("composite failed: ", e$message)
    NA
  })
  
  return(list(bp = bp, summary = summary_data))
}
