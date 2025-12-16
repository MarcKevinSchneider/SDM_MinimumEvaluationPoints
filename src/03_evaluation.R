#' @name 03_evaluation.R
#' @date 16.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Evaluation of each combination using AUC, TSS, RMSE, MAE, Pearson's Correlation,
#' Jaccard's Similarity Index and Sorensen's Similarity Index

# ================================================================
# 1. Load setup script and function script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# ================================================================
# 2. Evaluation
# ================================================================