#' @name 04_analysis.R
#' @date 12.03.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Analysis of the different sampling iterations concerning the minimum number
#' of validation points

devtools::install_github("etam4260/kneedle")
library(kneedle)


# ================================================================
# 1. Load setup script and function script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/SDM_MinimumEvaluationPoints/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# set seed
set.seed(2962)

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

# read PA data
pa_path <- paste0(envrmt$path_evaluation, "/PA/")
pa_df <- list.files(pa_path, full.names = TRUE) %>% map_df(readRDS)

# ================================================================
# 3. AUC overview plot for each species
# ================================================================

# number of n values per bin/boxplot
bin_size <- 5  

for (eval in c("PA", "PO_Balanced", "PO_Random")) {
  
  # plot directory
  plot_dir <- paste0(envrmt$path_evaluation, "/Plots/", eval)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  for (sp_name in species_list) {
    
    sp_data <- readRDS(paste0(envrmt$path_evaluation, "/", eval, "/", eval, "_", sp_name, ".rds"))
    
    # Bin the n values
    sp_data <- sp_data %>%
      dplyr::mutate(
        n = as.numeric(as.character(n)),
        n_bin = cut(n,breaks = seq(0, max(n) + bin_size, by = bin_size),
                    labels = FALSE) * bin_size, n_bin = factor(n_bin))
    
    # sort by fits
    fit_levels <- sort(unique(sp_data$fit))
    
    # plot auc for all n 
    plot_list <- lapply(fit_levels, function(f) {
      ggplot(sp_data %>% filter(fit == f), aes(x = n_bin, y = COR)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
        labs(title = paste("Fit:", f), x = "Sample size (n)", y = "COR") +
        ylim(0, 1) + theme_bw() +
        theme(
          plot.title  = element_text(size = 9, face = "bold"),
          axis.title  = element_text(size = 8),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
          axis.text.y = element_text(size = 7)
        )
    })
    
    # wrap into a 3x3 plot
    combined_plot <- wrap_plots(plot_list, ncol = 3) +
      plot_annotation(title = paste0(eval, " | Species: ", sp_name, " — COR by Sample Size and Fit"),
        theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5)))
    
    # save as png
    ggsave(filename = paste0(plot_dir, "/", eval, "_", sp_name, "_COR_boxplots.png"),
           plot = combined_plot, width    = 14, height   = 10, dpi = 200)
    
    message("Saved plot for ", eval, " | ", sp_name)
  }
}

# ================================================================
# 4. Overview plot over all species
# ================================================================

# loop over all evaluation methods
for (eval_type in c("PA", "PO_Balanced", "PO_Random")) {
  

  data_path <- paste0(envrmt$path_evaluation, "/", eval_type, "/")
  
  # read data
  full_df <- list.files(data_path, pattern = "\\.rds$", full.names = TRUE) %>% map_df(readRDS)
  
  # preparing the data
  full_df <- full_df %>%
    dplyr::mutate(
      n = as.numeric(as.character(n)),
      # create bins
      n_bin_val = cut(n, breaks = seq(0, max(n, na.rm = TRUE) + bin_size, by = bin_size),
                      labels = FALSE) * bin_size, 
      n_bin = factor(n_bin_val)
    )
  
  # plot dir
  plot_dir <- paste0(envrmt$path_evaluation, "/Plots/", eval_type)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # fits
  fit_levels <- sort(unique(full_df$fit))
  
  # plots
  plot_list <- lapply(fit_levels, function(f) {
    

    fit_data <- full_df %>% filter(fit == f)
    
    # boxplot for all fits over all species
    ggplot(fit_data, aes(x = n_bin, y = TSS)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
      # median as line as well
      stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred", size = 0.5) +
      labs(title = paste("Fit:", f), x = "Sample size (n)", y = "TSS") +
      ylim(0, 1) + 
      theme_bw() +
      theme(
        plot.title  = element_text(size = 10, face = "bold"),
        axis.title  = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.text.y = element_text(size = 7),
        panel.grid.minor = element_blank()
      )
  })
  
  # combine the plots and save
  combined_plot <- wrap_plots(plot_list, ncol = 3) +
    plot_annotation(
      title = paste0(eval_type, " | All Species — TSS"),,
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 11, hjust = 0.5))
    )
  
  # Speichern
  file_name <- paste0(plot_dir, "/", eval_type, "_ALL_SPECIES_TSS_boxplots.png")
  ggsave(filename = file_name, plot = combined_plot, width = 15, height = 11, dpi = 300)
  
  message("Saved combined plot for ", eval_type, " to ", file_name)
}


# ================================================================
# 5. Scatterplot over all species
# ================================================================


for (eval_type in c("PA", "PO_Balanced", "PO_Random")) {
  
  # read data for all species
  data_path <- paste0(envrmt$path_evaluation, "/", eval_type, "/")
  full_df <- list.files(data_path, pattern = "\\.rds$", full.names = TRUE) %>% 
    map_df(readRDS)
  
  full_df$n <- as.numeric(as.character(full_df$n))
  

  plot_dir <- paste0(envrmt$path_evaluation, "/Plots/", eval_type)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  fit_levels <- sort(unique(full_df$fit))
  
  # loop for the scattetrplots
  plot_list <- lapply(fit_levels, function(f) {
    
    fit_data <- full_df %>% filter(fit == f)
    
    ggplot(fit_data, aes(x = n, y = COR)) +
      geom_jitter(color = "steelblue", alpha = 0.1, size = 0.4, width = 1.5) +
      # trend line
      stat_smooth(method = "loess", color = "darkred", size = 0.8, se = TRUE) +
      labs(title = paste("Fit:", f), x = "Sample size (n)", y = "COR") +
      ylim(0, 1) + 
      theme_bw() +
      theme(
        plot.title  = element_text(size = 10, face = "bold"),
        axis.title  = element_text(size = 8),
        axis.text    = element_text(size = 7),
        panel.grid.minor = element_blank()
      )
  })
  
  # 3. combine plots and save
  combined_plot <- wrap_plots(plot_list, ncol = 3) +
    plot_annotation(
      title = paste0(eval_type, " | Combined Species Scatter Analysis — COR vs Sample Size"),
      subtitle = "Each point represents one simulation | Red line = LOESS Smooth Trend",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    )
  
  file_name <- paste0(plot_dir, "/", eval_type, "_ALL_SPECIES_COR_scatter.png")
  ggsave(filename = file_name, plot = combined_plot, width = 15, height = 11, dpi = 300)
  
  message("Saved scatter plot for ", eval_type)
}


# ================================================================
# 6. Analysis of minimum number of validation points
# ================================================================


get_knee_point <- function(n_values, metric_values) {
  
  # 1. Aggregating the data
  #--------------------------------------------------------
  summary_data <- data.frame(n = n_values, metric = metric_values) %>%
    dplyr::mutate(n = as.numeric(as.character(n))) %>% 
    dplyr::group_by(n) %>%
    dplyr::summarise(sd_val = sd(metric, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(sd_val)) %>%
    dplyr::arrange(n)
  
  # 2. Extract coordinates 
  #--------------------------------------------------------
  x <- summary_data$n
  y <- summary_data$sd_val
  
  
  # 3. Points of the connection line
  #--------------------------------------------------------
  p1 <- c(x[1], y[1])
  pn <- c(x[length(x)], y[length(y)])
  
  
  # 4. Distance of each point to the line
  #--------------------------------------------------------

  line_vec <- pn - p1
  
  # calculate square root for distance calculation
  distances <- sapply(1:length(x), function(i) {
    p_i <- c(x[i], y[i])
    abs(line_vec[1] * (p1[2] - p_i[2]) - (p1[1] - p_i[1]) * line_vec[2]) / 
      sqrt(sum(line_vec^2))
  })
  
  # get maximum distance point (knee)
  knee_index <- which.max(distances)
  
  return(x[knee_index])
}


get_knee_point_smooth <- function(n_values, metric_values, span = 0.1) {

  # 1. Aggregating the data
  #--------------------------------------------------------
  summary_data <- data.frame(n = n_values, metric = metric_values) %>%
    dplyr::mutate(n = as.numeric(as.character(n))) %>% 
    dplyr::group_by(n) %>%
    dplyr::summarise(sd_val = sd(metric, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::filter(!is.na(sd_val)) %>%
    dplyr::arrange(n)
  
  # 2. Smooth data using loess
  #--------------------------------------------------------
  smooth_mod <- loess(sd_val ~ n, data = summary_data, span = span)
  summary_data$sd_smooth <- predict(smooth_mod, summary_data$n)
  
  # coords of the cureve
  x <- summary_data$n
  y <- summary_data$sd_smooth
  
  # 3. Points of the connection line
  #--------------------------------------------------------
  p1 <- c(x[1], y[1])
  pn <- c(x[length(x)], y[length(y)])
  
  # 4. Distance of each point to the line
  #--------------------------------------------------------
  line_vec <- pn - p1
  
  # calculate square root for distance calculation
  distances <- sapply(1:length(x), function(i) {
    p_i <- c(x[i], y[i])
    abs(line_vec[1] * (p1[2] - p_i[2]) - (p1[1] - p_i[1]) * line_vec[2]) / 
      sqrt(sum(line_vec^2))
  })
  
  # get maximum distance point (knee)
  knee_index <- which.max(distances)
  return(x[knee_index])
}

test <- readRDS(paste0(envrmt$path_evaluation, "/PA/PA_VS10.rds"))

test$n <- as.numeric(as.character(test$n))

results <- list()


print(do.call(rbind, results))

for (f in fit_levels) {
  sub_data <- test %>% filter(fit == f)
  
  #kp_auc <- get_knee_point_smooth(sub_data$n, sub_data$AUC)
  #kp_cor <- get_knee_point_smooth(sub_data$n, sub_data$COR)
  #kp_tss <- get_knee_point_smooth(sub_data$n, sub_data$TSS)
  
  #kp_auc <- get_knee_point(sub_data$n, sub_data$AUC)
  #kp_cor <- get_knee_point(sub_data$n, sub_data$COR)
  #kp_tss <- get_knee_point(sub_data$n, sub_data$TSS)
  
  kp_auc <- get_stability_hybrid(sub_data$n, sub_data$AUC)
  kp_cor <- get_stability_hybrid(sub_data$n, sub_data$COR)
  kp_tss <- get_stability_hybrid(sub_data$n, sub_data$TSS)
  
  
  results[[as.character(f)]] <- data.frame(fit = f, knee_AUC = kp_auc, knee_COR = kp_cor,
                                           knee_TSS = kp_tss)
}

print(do.call(rbind, results))


# ================================================================
# 7. COR overview plot with Knee Point markers
# ================================================================

# number of n values per bin/boxplot
bin_size <- 5  

for (eval in c("PA", "PO_Balanced", "PO_Random")) {
  
  plot_dir <- paste0(envrmt$path_evaluation, "/Plots/", eval)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  for (sp_name in species_list) {
    
    sp_data <- readRDS(paste0(envrmt$path_evaluation, "/", eval, "/", eval, "_", sp_name, ".rds"))
    
    # ensure n is numeric
    sp_data$n <- as.numeric(as.character(sp_data$n))
    
    # Bin the n values for the boxplot x-axis
    sp_data <- sp_data %>%
      dplyr::mutate(
        n_bin_val = cut(n, breaks = seq(0, max(n) + bin_size, by = bin_size),
                        labels = FALSE) * bin_size, 
        n_bin = factor(n_bin_val))
    
    fit_levels <- sort(unique(sp_data$fit))
    
    plot_list <- lapply(fit_levels, function(f) {
      
      # Filter data for current fit
      curr_fit_data <- sp_data %>% filter(fit == f)
      
      # calc knee
      kp_val <- get_knee_point_smooth(curr_fit_data$n, curr_fit_data$COR, span = 0.1)
      
      # round to nearest bin size to get vertical line coords
      kp_bin <- round(kp_val / bin_size) * bin_size
      
      ggplot(curr_fit_data, aes(x = n_bin, y = COR)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
        # Add the vertical dashed line at the knee point
        geom_vline(aes(xintercept = factor(kp_bin)), 
                   color = "firebrick", linetype = "dashed", size = 0.8) +
        # add n value
        annotate("text", x = factor(kp_bin), y = 0.05, label = paste0("n=", round(kp_val)), 
                 color = "firebrick", angle = 90, vjust = -0.5, size = 2.5, fontface = "bold") +
        labs(title = paste("Fit:", f), x = "Sample size (n)", y = "COR") +
        ylim(0, 1) + theme_bw() +
        theme(
          plot.title  = element_text(size = 9, face = "bold"),
          axis.title  = element_text(size = 8),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
          axis.text.y = element_text(size = 7)
        )
    })
    
    combined_plot <- wrap_plots(plot_list, ncol = 3) +
      plot_annotation(title = paste0(eval, " | Species: ", sp_name, " — COR by Sample Size and Fit (Red dashed line = Knee Point)"),
                      theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5)))
    
    ggsave(filename = paste0(plot_dir, "/", eval, "_", sp_name, "_COR_boxplots_with_Knee.png"),
           plot = combined_plot, width = 14, height = 10, dpi = 200)
    
    message("Saved plot with Knee Points for ", eval, " | ", sp_name)
  }
}

# ================================================================
# 8. Static threshold for all species
# ================================================================

# using the threshold from Silvey & Liu 2024 but adjusted upwards slightly
# http://dx.doi.org/10.2196/60231
#for (eval in c("PA", "PO_Balanced", "PO_Random")) {
for (eval in c("PO_Random")) {
  
  plot_dir <- paste0(envrmt$path_evaluation, "/Plots/", eval)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  for (sp_name in species_list) {
    
    sp_data <- readRDS(paste0(envrmt$path_evaluation, "/", eval, "/", eval, "_", sp_name, ".rds"))
    
    sp_data$n <- as.numeric(as.character(sp_data$n))
    
    sp_data <- sp_data %>%
      dplyr::mutate(
        n_bin_val = cut(n, breaks = seq(0, max(n) + bin_size, by = bin_size),
                        labels = FALSE) * bin_size,
        n_bin = factor(n_bin_val))
    
    # compute stability thresholds for median
    summary_df <- sp_data %>%
      dplyr::mutate(n = as.numeric(as.character(n))) %>%
      dplyr::group_by(fit, n) %>%
      dplyr::summarise(median_val = median(COR, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(!is.na(median_val)) %>%
      dplyr::arrange(fit, n)
    
    # since values for the random background points increase with sample size n
    # instead of decreasing with increasing n like for PA or balanced background points
    if (eval == "PO_Random"){
      stability_results <- summary_df %>%
        dplyr::group_by(fit) %>%
        dplyr::mutate(
          ref_val   = median_val[n == 300],
          threshold = ref_val - 0.04,
          within    = median_val <= threshold
        ) %>%
        dplyr::arrange(n) %>%
        dplyr::mutate(
          stays_within = rev(cumall(rev(within)))
        ) %>%
        dplyr::summarise(
          ref_val      = dplyr::first(ref_val),
          threshold    = dplyr::first(threshold),
          min_n_stable = max(n[stays_within], na.rm = TRUE),
          .groups      = "drop"
        )
    } else {
      stability_results <- summary_df %>%
        dplyr::group_by(fit) %>%
        dplyr::mutate(
          ref_val   = median_val[n == 300],
          threshold = ref_val + 0.04,
          within    = median_val <= threshold
        ) %>%
        dplyr::arrange(n) %>%
        dplyr::mutate(
          stays_within = rev(cumall(rev(within)))
        ) %>%
        dplyr::summarise(
          ref_val      = dplyr::first(ref_val),
          threshold    = dplyr::first(threshold),
          min_n_stable = min(n[stays_within], na.rm = TRUE),
          .groups      = "drop"
        )
    }
    
    fit_levels <- sort(unique(sp_data$fit))
    
    plot_list <- lapply(fit_levels, function(f) {
      
      curr_fit_data <- sp_data %>% dplyr::filter(fit == f)
      
      # Get stability n for this fit
      stab_val <- stability_results %>% 
        dplyr::filter(fit == f) %>% 
        dplyr::pull(min_n_stable)
      
      # Round to nearest bin
      stab_bin <- round(stab_val / bin_size) * bin_size
      
      # if the threshold rounds to 0 then set the placement to 5 for this graphic
      # otherwise the threshold will be bugged
      if (stab_bin == 0){
        stab_bin = 5
      }
      
      ggplot(curr_fit_data, aes(x = n_bin, y = COR)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
        geom_vline(aes(xintercept = factor(stab_bin)),
                   color = "firebrick", linetype = "dashed", size = 0.8) +
        annotate("text", x = factor(stab_bin), y = 0.05,
                 label = paste0("n=", stab_val),
                 color = "firebrick", angle = 90, vjust = -0.5, size = 2.5, fontface = "bold") +
        labs(title = paste("Fit:", f), x = "Sample size (n)", y = "COR") +
        ylim(0, 1) + theme_bw() +
        theme(
          plot.title  = element_text(size = 9, face = "bold"),
          axis.title  = element_text(size = 8),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
          axis.text.y = element_text(size = 7)
        )
    })
    
    combined_plot <- wrap_plots(plot_list, ncol = 3) +
      plot_annotation(
        title = paste0(eval, " | Species: ", sp_name, " — COR by Sample Size and Fit (Red dashed line = Stability Threshold)"),
        theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
      )
    
    ggsave(filename = paste0(plot_dir, "/", eval, "_", sp_name, "_COR_boxplots_with_Stability.png"),
           plot = combined_plot, width = 14, height = 10, dpi = 200)
    
    message("Saved plot with Stability Thresholds for ", eval, " | ", sp_name)
  }
}


# ================================================================
# 9. Static threshold over all species
# ================================================================

for (eval_type in c("PA", "PO_Balanced", "PO_Random")) {
  
  data_path <- paste0(envrmt$path_evaluation, "/", eval_type, "/")
  
  full_df <- list.files(data_path, pattern = "\\.rds$", full.names = TRUE) %>% map_df(readRDS)
  
  full_df <- full_df %>%
    dplyr::mutate(
      n = as.numeric(as.character(n)),
      n_bin_val = cut(n, breaks = seq(0, max(n, na.rm = TRUE) + bin_size, by = bin_size),
                      labels = FALSE) * bin_size,
      n_bin = factor(n_bin_val)
    )
  
  # compute threshold
  summary_df <- full_df %>%
    dplyr::group_by(fit, n) %>%
    dplyr::summarise(median_val = median(COR, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(median_val)) %>%
    dplyr::arrange(fit, n)
  
  stability_results <- summary_df %>%
    dplyr::group_by(fit) %>%
    dplyr::mutate(
      ref_val      = median_val[n == 300],
      threshold    = ref_val + 0.04,
      within       = median_val <= threshold,
      stays_within = rev(cumall(rev(within)))
    ) %>%
    dplyr::summarise(
      ref_val      = dplyr::first(ref_val),
      threshold    = dplyr::first(threshold),
      min_n_stable = min(n[stays_within], na.rm = TRUE),
      .groups      = "drop"
    )
  
  plot_dir <- paste0(envrmt$path_evaluation, "/Plots/", eval_type)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  fit_levels <- sort(unique(full_df$fit))
  
  plot_list <- lapply(fit_levels, function(f) {
    
    fit_data <- full_df %>% dplyr::filter(fit == f)
    
    stab_val <- stability_results %>%
      dplyr::filter(fit == f) %>%
      dplyr::pull(min_n_stable)
    
    stab_bin <- round(stab_val / bin_size) * bin_size
    
    if (stab_bin == 0){
      stab_bin = 5
    }
    
    ggplot(fit_data, aes(x = n_bin, y = COR)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
      stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred", size = 0.5) +
      geom_vline(aes(xintercept = factor(stab_bin)),
                 color = "firebrick", linetype = "dashed", size = 0.8) +
      annotate("text", x = factor(stab_bin), y = 0.05,
               label = paste0("n=", stab_val),
               color = "firebrick", angle = 90, vjust = -0.5, size = 2.5, fontface = "bold") +
      labs(title = paste("Fit:", f), x = "Sample size (n)", y = "COR") +
      ylim(0, 1) +
      theme_bw() +
      theme(
        plot.title       = element_text(size = 10, face = "bold"),
        axis.title       = element_text(size = 8),
        axis.text.x      = element_text(angle = 90, hjust = 1, size = 6),
        axis.text.y      = element_text(size = 7),
        panel.grid.minor = element_blank()
      )
  })
  
  combined_plot <- wrap_plots(plot_list, ncol = 3) +
    plot_annotation(
      title = paste0(eval_type, " | All Species — COR (Red dashed line = Stability Threshold)"),
      theme = theme(plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 11, hjust = 0.5))
    )
  
  file_name <- paste0(plot_dir, "/", eval_type, "_ALL_SPECIES_COR_boxplots_with_Stability.png")
  ggsave(filename = file_name, plot = combined_plot, width = 15, height = 11, dpi = 300)
  
  message("Saved combined plot for ", eval_type, " to ", file_name)
}


# ================================================================
# 10. Threshold using segmented regression
# ================================================================

library(segmented)

get_breakpoint <- function(n_values, metric_values) {
  
  # 1. Aggregate data
  #--------------------------------------------------------
  summary_data <- data.frame(n = n_values, metric = metric_values) %>%
    dplyr::mutate(n = as.numeric(as.character(n))) %>% 
    dplyr::group_by(n) %>%
    dplyr::summarise(median_val = median(metric, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::filter(!is.na(median_val))
  
  # 2. Standard linear regression
  #--------------------------------------------------------
  lin_mod <- lm(median_val ~ n, data = summary_data)
  

  # 3. Trying the segmented regression
  #--------------------------------------------------------
  tryCatch({
    seg_mod <- segmented(lin_mod, seg.Z = ~n, psi = list(n = median(summary_data$n)))
    
    # extract break point
    bp <- seg_mod$psi[1, 2]
    
    return(bp)
  }, error = function(e) {
    return(NA) 
  })
}


for (eval_type in c("PA", "PO_Balanced", "PO_Random")) {
  
  data_path <- paste0(envrmt$path_evaluation, "/", eval_type, "/")
  full_df <- list.files(data_path, pattern = "\\.rds$", full.names = TRUE) %>% 
    map_df(readRDS)
  
  full_df <- full_df %>%
    dplyr::mutate(
      n = as.numeric(as.character(n)),
      n_bin_val = cut(n, breaks = seq(0, max(n, na.rm = TRUE) + bin_size, by = bin_size),
                      labels = FALSE) * bin_size,
      n_bin = factor(n_bin_val)
    )
  
  # compute threshold with the segmented regression
  stability_results <- full_df %>%
    dplyr::group_by(fit) %>%
    dplyr::summarise(
      min_n_stable = get_breakpoint(n, COR),
      .groups = "drop"
    ) %>%
    dplyr::mutate(min_n_stable = round(min_n_stable))
  
  plot_dir <- paste0(envrmt$path_evaluation, "/Plots/", eval_type)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  fit_levels <- sort(unique(full_df$fit))
  
  plot_list <- lapply(fit_levels, function(f) {
    
    fit_data <- full_df %>% dplyr::filter(fit == f)
    
    # Extract the calculated breakpoint
    stab_val <- stability_results %>%
      dplyr::filter(fit == f) %>%
      dplyr::pull(min_n_stable)
    
    # If the model fails
    if(is.na(stab_val)) return(NULL) 
    
    stab_bin <- round(stab_val / bin_size) * bin_size
    
    if (stab_bin == 0){
      stab_bin = 5
    }
    
    ggplot(fit_data, aes(x = n_bin, y = COR)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
      stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred", size = 0.5) +
      geom_vline(aes(xintercept = factor(stab_bin)),
                 color = "firebrick", linetype = "dashed", size = 0.8) +
      annotate("text", x = factor(stab_bin), y = 0.05,
               label = paste0("BP n=", round(stab_val)),
               color = "firebrick", angle = 90, vjust = -0.5, size = 2.5, fontface = "bold") +
      labs(title = paste("Fit:", f), x = "Sample size (n)", y = "COR") +
      ylim(0, 1) +
      theme_bw() +
      theme(
        plot.title       = element_text(size = 10, face = "bold"),
        axis.title       = element_text(size = 8),
        axis.text.x      = element_text(angle = 90, hjust = 1, size = 6),
        axis.text.y      = element_text(size = 7),
        panel.grid.minor = element_blank()
      )
  })
  
  plot_list <- Filter(Negate(is.null), plot_list)
  
  combined_plot <- wrap_plots(plot_list, ncol = 3) +
    plot_annotation(
      title = paste0(eval_type, " | Segmented Regression Breakpoints (COR)"),
      theme = theme(plot.title    = element_text(size = 14, face = "bold", hjust = 0.5))
    )
  
  file_name <- paste0(plot_dir, "/", eval_type, "_COR_Segmented_Stability.png")
  ggsave(filename = file_name, plot = combined_plot, width = 15, height = 11, dpi = 300)
}



