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
# 2. Breakpoint function
# ================================================================

get_breakpoint <- function(n_values, metric_values, pct = 0.99) {
  
  # 1. Aggregate to median and IQR per n
  #--------------------------------------------------------
  summary_data <- data.frame(n = n_values, metric = metric_values) %>%
    dplyr::mutate(n = as.numeric(as.character(n))) %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(
      # median for the calculation
      med_val = median(metric, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(med_val)) %>%
    dplyr::arrange(n)
  
  
  # 2. Fit exponential to median: med ~ a * exp(b * n) + c
  #--------------------------------------------------------
  
  # get first and last value 
  y_start <- summary_data$med_val[1]
  y_end   <- summary_data$med_val[nrow(summary_data)]
  
  # get direction of curve
  # necessary because some metrics are rising and some are falling
  is_rising <- y_end > y_start
  
  start_vals <- list(a = y_start - y_end, b = ifelse(is_rising, -0.05, -0.05), 
                     c = y_end)
  
  bp <- tryCatch({
    
    # 3. Fit the model
    #--------------------------------------------------------
    exp_mod <- nls(med_val ~ a * exp(b * n) + c, data = summary_data, 
                   start = start_vals, control = nls.control(maxiter = 500, tol = 1e-4))
    
    # get coefficient
    cf <- coef(exp_mod)
    
    
    # 4. Fitted curve range from n_min to n_max
    #--------------------------------------------------------
    
    # fit along the 300 n
    n_seq      <- seq(min(summary_data$n), max(summary_data$n), by = 1)
    fitted_seq <- cf["a"] * exp(cf["b"] * n_seq) + cf["c"]
    
    # calc total change per n 
    total_change <- abs(fitted_seq[length(fitted_seq)] - fitted_seq[1])
    
    # calc where target change is achieved
    # so like where 99% of change is done
    target_change <- pct * total_change
    
    # walk along fitted curve until we hit the target change
    delta <- abs(fitted_seq - fitted_seq[1])
    bp_idx <- which(delta >= target_change)[1]
    # get n for thatr point
    n_bp <- n_seq[bp_idx]
    
    n_bp
    
  }, error = function(e) {
    message("nls failed: ", e$message)
    NA
  })
  
  return(list(bp = bp, summary = summary_data))
}