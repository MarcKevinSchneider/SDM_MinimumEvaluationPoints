#' @name evaluation_function.R
#' @date 16.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for evaluating the samples using AUC, TSS, RMSE, MAE, 
#' Pearson's Correlation, Jaccard's Similarity Index and Sorensen's Similarity Index

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
  '
  Purpose: Helper function for calculating the AUC
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  AUC
  '
  AUC <- Metrics::auc(actual = df$Observed, predicted = df$Predicted)
  
  return(AUC)
}

# 2 - MAE ####
#-----------------------------------------#

mae_eval <- function(df){
  '
  Purpose: Helper function for calculating the MAE
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  MAE
  '
  MAE <- Metrics::mae(actual = df$Observed, predicted = df$Predicted)
  
  return(MAE)
}


# 3 - MSE ####
#-----------------------------------------#

rmse_eval <- function(df){
  '
  Purpose: Helper function for calculating the RMSE
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  RMSE
  '
  RMSE <- Metrics::rmse(actual = df$Observed, predicted = df$Predicted)
  
  return(RMSE)
}

# 4 - TSS ###
#-----------------------------------------#

tss_eval <- function(df){
  '
  Purpose: Helper function for calculating the TSS
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  TSS
  '
  # true positives
  tp <- sum(df$Predicted == 1 & df$Observed == 1)
  # false negatives
  fn <- sum(df$Predicted == 0 & df$Observed == 1)
  # false positives
  fp <- sum(df$Predicted == 1 & df$Observed == 0)
  # true negatives
  tn <- sum(df$Predicted == 0 & df$Observed == 0)
  
  # formula for calculating TSS
  # have to do it this way since "Metrics" doesnt have a TSS function
  TSS <- (tp / (tp + fn)) - (fp / (fp + tn))
  
  return(TSS)
}


# 5 - Pearson's Correlation Coefficient ###
#-----------------------------------------#

cor_eval <- function(df){
  '
  Purpose: Helper function for calculating Pearsons Correlation Coefficient
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  Pearsons R
  '
  pearson_r <- cor(
    as.numeric(df$Predicted),
    as.numeric(df$Observed),
    method = "pearson",
    use = "complete.obs"
  )
}

# 6 - Jaccard's Similarity Index ###
#-----------------------------------------#

# from https://www.r-bloggers.com/2021/11/how-to-calculate-jaccard-similarity-in-r-2/

jaccard_eval <- function(df){
  '
  Purpose: Helper function for calculating Jaccards Similarity Index
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  Jaccards Similarity Index
  '
  
  # apparently this is not correct for binary data?
  # have to check again
  #intersection = length(intersect(df$Predicted, df$Observed))
  #union = length(df$Predicted) + length(df$Observed) - intersection
  #JAC <- intersection/union
  
  # true positive
  tp <- sum(df$Predicted == 1 & df$Observed == 1)
  # false positive
  fp <- sum(df$Predicted == 1 & df$Observed == 0)
  # false negative
  fn <- sum(df$Predicted == 0 & df$Observed == 1)
  
  JAC <- tp / (tp + fp + fn)
  return(JAC)

}

# 7 - Jaccard's Dissimilarity Index ###
#-----------------------------------------#

# just to have it in case it is needed
# also from https://www.r-bloggers.com/2021/11/how-to-calculate-jaccard-similarity-in-r-2/

jaccard_distance <- function(df){
  '
  Purpose: Helper function for calculating Jaccards Dissimilarity Index
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  Jacards Dissimilarity Index
  '
  jacard <- jacard_eval(df)
  JAC_DIS <- 1 - jacard
  return(JAC_DIS)
}

# 7 - Sorensen's Similarity Index ###
#-----------------------------------------#

sorensen_eval <- function(df){
  '
  Purpose: Helper function for calculating Sorensens Similarity Index
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  Sorensens Similarity Index
  '
  # true positive
  tp <- sum(df$Predicted == 1 & df$Observed == 1)
  # false positive
  fp <- sum(df$Predicted == 1 & df$Observed == 0)
  # false negative
  fn <- sum(df$Predicted == 0 & df$Observed == 1)
  
  # calc sorensen's
  SOREN <- (2 * tp) / (2 * tp + fp + fn)
  return(SOREN)
}

# 8 - Function for executing all metrics ##
#-----------------------------------------#

eval_funcs <- function(df){
  '
  Purpose: Helper function for calculating all evaluation metrics
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "predicted" and "observed
    
  
  Returns:
  --------------------------
  List of all evaluation metrics
  '
  # AUC
  AUC <- auc_eval(df)
  # MAE
  MAE <- mae_eval(df)
  # RMSE
  RMSE <- rmse_eval(df)
  # TSS
  TSS <- tss_eval(df)
  # Pearson R
  COR <- cor_eval(df)
  # Jaccards Similarity index
  JAC <- jaccard_eval(df)
  # Jaccards Dissimilarity Index
  DIS <- jaccard_distance(df)
  # Sorensens Similarity Index
  SOR <- sorensen_eval(df)
  
  return(list(AUC=AUC, MAE=MAE, RMSE=RMSE, TSS=TSS, COR=COR,
              JAC=JAC, DIS=DIS, SOR=SOR))
}
