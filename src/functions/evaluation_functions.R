#' @name evaluation_function.R
#' @date 16.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions evaluating the samples using AUC, TSS, RMSE, MAE, Pearson's Correlation,
#' Jaccard's Similarity Index and Sorensen's Similarity Index

# ================================================================
# 1. Load setup script
# ================================================================

rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = FALSE) # echo set to false here to stop the script from printing

set.seed(2962)

# ================================================================
# 2. Evaluation functions
# ================================================================

# first few metrics from https://cran.r-project.org/web/packages/Metrics/Metrics.pdf

# 1 - AUC ####
#-----------------------------------------#

auc_eval <- function(df){
  AUC <- Metrics::auc(actual = df$observed, predicted = df$predicted)
  
  return(AUC)
}

# 2 - MAE ####
#-----------------------------------------#

mae_eval <- function(df){
  MAE <- Metrics::mae(actual = df$observed, predicted = df$predicted)
  
  return(MAE)
}


# 3 - MSE ####
#-----------------------------------------#

rmse_eval <- function(df){
  RMSE <- Metrics::rmse(actual = df$observed, predicted = df$predicted)
  
  return(RMSE)
}

# 4 - TSS ###
#-----------------------------------------#

tss_eval <- function(df){
  # true positives
  tp <- sum(df$predicted == 1 & df$observed == 1)
  # false negatives
  fn <- sum(df$predicted == 0 & df$observed == 1)
  # false positives
  fp <- sum(df$predicted == 1 & df$observed == 0)
  # true negatives
  tn <- sum(df$predicted == 0 & df$observed == 0)
  
  # formula for calculating TSS
  # have to do it this way since "Metrics" doesnt have a TSS function
  TSS <- (tp / (tp + fn)) - (fp / (fp + tn))
  
  return(TSS)
}


# 5 - Pearson's Correlation Coefficient ###
#-----------------------------------------#

cor_eval <- function(df){
  pearson_r <- cor(
    as.numeric(df$predicted),
    as.numeric(df$observed),
    method = "pearson",
    use = "complete.obs"
  )
}

# 6 - Jaccard's Similarity Index ###
#-----------------------------------------#

# from https://www.r-bloggers.com/2021/11/how-to-calculate-jaccard-similarity-in-r-2/

jacard_eval <- function(df){
  
  # apparently this is not correct for binary data?
  # have to check again
  #intersection = length(intersect(df$predicted, df$observed))
  #union = length(df$predicted) + length(df$observed) - intersection
  #JAC <- intersection/union
  
  # true positive
  tp <- sum(df$predicted == 1 & df$observed == 1)
  # false positive
  fp <- sum(df$predicted == 1 & df$observed == 0)
  # false negative
  fn <- sum(df$predicted == 0 & df$observed == 1)
  
  JAC <- tp / (tp + fp + fn)
  return(JAC)

}

# 7 - Jaccard's Dissimilarity Index ###
#-----------------------------------------#

# just to have it in case it is needed
# also from https://www.r-bloggers.com/2021/11/how-to-calculate-jaccard-similarity-in-r-2/

jacard_distance <- function(df){
  jacard <- jacard_eval(df)
  JAC_DIS <- 1 - jacard
  return(JAC_DIS)
}

# 7 - Sorensen's Similarity Index ###
#-----------------------------------------#

sorensen_eval <- function(df){
  # true positive
  tp <- sum(df$predicted == 1 & df$observed == 1)
  # false positive
  fp <- sum(df$predicted == 1 & df$observed == 0)
  # false negative
  fn <- sum(df$predicted == 0 & df$observed == 1)
  
  # calc sorensen's
  SOREN <- (2 * tp) / (2 * tp + fp + fn)
  return(SOREN)
}
