#' @name 04_analysis.R
#' @date 12.03.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Analysis of the different sampling iterations concerning the minimum number
#' of validation points

# ================================================================
# 1. Load setup script and function script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# set seed
set.seed(2962)

# sourcing the breakpoint function
source(paste0(envrmt$path_src, "/functions/get_breakpoint_function.R"))

# ================================================================
# 2. Preparing the data
# ================================================================

# species list
species_list <- c("VS01", "VS02", "VS03", "VS04", "VS05", 
                  "VS06", "VS07", "VS08", "VS09", "VS10")

for (eval in c("PA", "PO_Balanced", "PO_Random")){
  df <- readRDS(paste0(envrmt$path_evaluation, "/", eval, "_Evaluation_combined.rds"))
  
  # round eval metrics so that we can save some disk space
  df <- df %>%
    dplyr::mutate(across(c(AUC, MAE, RMSE, TSS, COR, JAC, DIS, SOR), ~ round(.x, 3)))
  
  dir <- paste0(envrmt$path_evaluation, "/", eval)
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  # filter for each species and save to a new folder
  for (sp_name in species_list){
    sp_data <- df %>% filter(sp == sp_name)
    saveRDS(sp_data, file = paste0(dir, "/", eval, "_", sp_name, ".rds"))
  }
}

# ================================================================
# 3. Overview boxplot over all strats, fits and virtual species
# ================================================================

# bin size for the boxplots
bin_size <- 10

# eval metrics
metrics <- c("AUC", "MAE", "RMSE", "TSS", "COR", "JAC", "DIS", "SOR")

for (eval in c("PA", "PO_Random", "PO_Balanced")){
  # read data
  data_path <- paste0(envrmt$path_evaluation, "/", eval, "/")
  pa_df <- list.files(data_path, full.names = TRUE) %>% map_df(readRDS)
  
  # format data
  pa_df <- pa_df %>%
    dplyr::mutate(
      n = as.numeric(as.character(n)),
      n_bin_val = cut(n, breaks = seq(0, max(n, na.rm = TRUE) + bin_size, by = bin_size),
                      labels = FALSE) * bin_size,
      n_bin = factor(n_bin_val)
    )
  
  metric_plots <- lapply(metrics, function(metric) {
    
    plot_data <- pa_df %>% dplyr::filter(!is.na(.data[[metric]]))
    
    # 1. Get breakpoint using exponential decay method
    #--------------------------------------------------------
    bp <- get_breakpoint(n_values = plot_data$n, metric_values = plot_data[[metric]])
    stab_val <- bp$bp
    
    # mapping the stability value onto the bins
    stab_bin <- if (!is.na(stab_val)) {
      bins <- as.numeric(levels(plot_data$n_bin))
      bins[which.min(abs(bins - stab_val))]
    } else NA
    
    # 2. Plotting
    #--------------------------------------------------------
    p <- ggplot(plot_data, aes(x = n_bin, y = .data[[metric]])) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5) +
      stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred") +
      theme_bw() +
      theme(
        plot.title       = element_text(size = 9, face = "bold", hjust = 0.5),
        axis.title       = element_text(size = 7),
        axis.text.x      = element_text(angle = 90, hjust = 1, size = 5),
        axis.text.y      = element_text(size = 6),
        panel.grid.minor = element_blank()
      ) +
      xlab("n") +
      ggtitle(metric)
    
    if (!is.na(stab_bin)) {
      p <- p +
        geom_vline(xintercept = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                   color = "firebrick", linetype = "dashed") +
        annotate("text", x = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                 y = min(plot_data[[metric]], na.rm = TRUE),
                 label = paste0("n=", round(stab_val)), angle = 90, vjust = 1.5, 
                 size = 2.5, color = "darkred")
    }
    return(p)
  })
  
  # combine and save
  combined_plot <- wrap_plots(metric_plots, ncol = 4)
  
  ggsave(
    filename = paste0(envrmt$path_evaluation, "/Plots/", eval, "_FittedCurve_Threshold.png"),
    plot     = combined_plot,
    width    = 16, height = 8, dpi = 300
  )
}

