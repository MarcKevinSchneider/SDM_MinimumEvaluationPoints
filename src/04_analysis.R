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
  df <- readRDS(paste0(envrmt$path_evaluation, "/", eval, "_Evaluation_combined_new.rds"))
  
  # round eval metrics so that we can save some disk space
  df <- df %>% dplyr::mutate(across(c(AUC, MAE, RMSE, TSS, COR, JAC, DIS, SOR), ~ round(.x, 3)))
  
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
metrics <- c("AUC", "TSS", "KAP", "COR", "MAE", "RMSE", "JAC", "SOR")


#for (eval in c("PA")){
for (eval in c("PA", "PO_Random", "PO_Balanced")){  

  # read data
  data_path <- paste0(envrmt$path_evaluation, "/", eval, "/")
  pa_df <- list.files(data_path, full.names = TRUE) %>% map_df(readRDS)
  
  # format data
  pa_df <- pa_df %>% dplyr::mutate(n = as.numeric(as.character(n)),
                                   n_bin_val = cut(n, breaks = seq(0, max(n, na.rm = TRUE) + bin_size, 
                                                                   by = bin_size), labels = FALSE) * bin_size, 
                                   n_bin = factor(n_bin_val))
  
  metric_plots <- lapply(metrics, function(metric) {
    
    plot_data <- pa_df %>% dplyr::filter(!is.na(.data[[metric]]))
    
    # 1. Get breakpoint using exponential decay method
    #--------------------------------------------------------
    bp <- get_breakpoint(n_values = plot_data$n, metric_values = plot_data[[metric]],  slope_pct = 0.10,
                           weights = c(med = 0.5, range = 0.5))
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
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid.minor = element_blank()
      ) + xlab("n") + ggtitle(metric)
    
    if (!is.na(stab_bin)) {
      p <- p +
        geom_vline(xintercept = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                   color = "firebrick", linetype = "dashed", linewidth = 1) +
        annotate("text", x = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))), 
                 y = (min(plot_data[[metric]], na.rm = TRUE) + 0.1), label = paste0("n=", round(stab_val)), 
                 angle = 90, vjust = 2.5 ,size = 4.5, color = "darkred")
    }
    return(p)
  })
  
  # combine and save
  combined_plot <- wrap_plots(metric_plots, ncol = 2)
  
  ggsave(filename = paste0(envrmt$path_evaluation, "/Plots/", eval, "_FittedCurve_Threshold.png"),
         plot = combined_plot, width = 12, height = 16, dpi = 300)
}



# ================================================================
# 4. AUC threshold by niche breadth
# ================================================================

# read and combine all three datasets
combined_df <- dplyr::bind_rows(
  lapply(c("PA", "PO_Random", "PO_Balanced"), function(eval) {
    data_path <- paste0(envrmt$path_evaluation, "/", eval, "/")
    list.files(data_path, full.names = TRUE) %>%
      map_df(readRDS) %>%
      dplyr::mutate(eval = eval)}))

# niche breadth grouping
niche_groups <- list("Broad niche" = c("VS01", "VS02", "VS03"),
                     "Medium niche" = c("VS04", "VS05", "VS06"),
                     "Narrow niche" = c("VS07", "VS08", "VS09", "VS10"))

# assings the niche to each species
sp_to_niche <- tibble::enframe(niche_groups, name = "niche", value = "sp") %>% tidyr::unnest(sp)

# format and filter
combined_df <- combined_df %>%
  dplyr::mutate(n = as.numeric(as.character(n))) %>%
  dplyr::filter(n >= 1, n <= 300, !is.na(AUC)) %>%
  dplyr::mutate(n_bin_val = cut(n, breaks = seq(0, max(n, na.rm = TRUE) + bin_size, by = bin_size),
                                labels = FALSE) * bin_size, n_bin = factor(n_bin_val)) %>%
  dplyr::left_join(sp_to_niche, by = "sp") %>%
  dplyr::mutate(niche = factor(niche, levels = names(niche_groups)), 
                eval = factor(eval, levels = c("PA", "PO_Random", "PO_Balanced")))

# one plot per eval x niche combination
all_results <- lapply(levels(combined_df$eval), function(eval_label) {
  lapply(levels(combined_df$niche), function(niche_label) {
    
    plot_data <- combined_df %>% 
      dplyr::filter(eval == eval_label, niche == niche_label)
    
    bp <- get_breakpoint(n_values = plot_data$n, metric_values = plot_data$AUC)
    stab_val <- bp$bp
    
    stab_bin <- if (!is.na(stab_val)) {
      bins <- as.numeric(levels(plot_data$n_bin))
      bins[which.min(abs(bins - stab_val))]
    } else NA
    
    p <- ggplot(plot_data, aes(x = n_bin, y = AUC)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5) +
      stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred") +
      theme_bw() +
      ylim(0, 1) +
      theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 7),
            axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
            axis.text.y = element_text(size = 6),
            panel.grid.minor = element_blank()) +
      labs(x = "n", y = "AUC", title = niche_label)
    
    if (!is.na(stab_bin)) {
      p <- p +
        geom_vline(xintercept = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                   color = "firebrick", linetype = "dashed") +
        annotate("text",
                 x = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                 y = min(plot_data$AUC, na.rm = TRUE),
                 label = paste0("n=", round(stab_val)),
                 angle = 90, vjust = 1.5, size = 2.5, color = "darkred")
    }
    
    list(plot = p, eval = eval_label, niche = niche_label, stab_val = stab_val)
  })
})

# flatten nested list
all_results_flat <- unlist(all_results, recursive = FALSE)

# split plots and thresholds
all_plots <- lapply(all_results_flat, function(x) x$plot)

threshold_df <- tibble::tibble(eval = sapply(all_results_flat, function(x) x$eval),
                               niche = sapply(all_results_flat, function(x) x$niche),
                               stab_val = sapply(all_results_flat, function(x) x$stab_val))

write.csv(threshold_df, file = paste0(envrmt$path_evaluation, "/Plots/All_AUC_Thresholds_by_Niche.csv"),
          row.names = FALSE)

# helper function to create a rotated row label
row_label <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label,
             angle = 90, size = 4, fontface = "bold") +
    theme_void()
}

# plots for pa, po random and po balanced
row_PA <- wrap_plots(row_label("PA"),
                     wrap_plots(all_plots[1:3], ncol = 3), 
                     ncol = 2, widths = c(0.01, 1))

row_PO_Bal <- wrap_plots(row_label("PO-Balanced"),
                         wrap_plots(all_plots[7:9], ncol = 3),
                         ncol = 2, widths = c(0.01, 1))

row_PO_Rand <- wrap_plots(row_label("PO-Random"), 
                          wrap_plots(all_plots[4:6], ncol = 3),
                          ncol = 2, widths = c(0.01, 1))

combined_plot <- row_PA / row_PO_Bal / row_PO_Rand

ggsave(filename = paste0(envrmt$path_evaluation, "/Plots/All_AUC_by_Niche.png"),
       plot = combined_plot, width = 17, height = 14, dpi = 300)

# ================================================================
# 5. AUC threshold by fit category combined
# ================================================================

# read and combine all three datasets
combined_df <- dplyr::bind_rows(
  lapply(c("PA", "PO_Random", "PO_Balanced"), function(eval) {
    data_path <- paste0(envrmt$path_evaluation, "/", eval, "/")
    list.files(data_path, full.names = TRUE) %>%
      map_df(readRDS) %>%
      dplyr::mutate(eval = eval)
  })
)

# fit quality grouping
fit_groups <- list("Poor fit" = c(0.1, 0.2, 0.3), 
                   "Moderate fit" = c(0.4, 0.5, 0.6),
                   "Good fit" = c(0.7, 0.8, 0.9))

#  fit to quality label
fit_to_quality <- tibble::enframe(fit_groups, name = "quality", value = "fit") %>% tidyr::unnest(fit)

# format and filter
combined_df <- combined_df %>%
  dplyr::mutate(n = as.numeric(as.character(n)), fit = as.numeric(as.character(fit))) %>%
  dplyr::filter(n >= 1, n <= 300, !is.na(AUC)) %>%
  dplyr::mutate(n_bin_val = cut(n, breaks = seq(0, max(n, na.rm = TRUE) + bin_size, by = bin_size),
                                labels = FALSE) * bin_size, n_bin = factor(n_bin_val)) %>%
  dplyr::left_join(fit_to_quality, by = "fit") %>%
  dplyr::mutate(quality = factor(quality, levels = names(fit_groups)),
                eval = factor(eval, levels = c("PA", "PO_Random", "PO_Balanced")))

# one plot per eval x quality combination
all_results <- lapply(levels(combined_df$eval), function(eval_label) {
  lapply(levels(combined_df$quality), function(quality_label) {
    
    plot_data <- combined_df %>%
      dplyr::filter(eval == eval_label, quality == quality_label)
    
    bp <- get_breakpoint(n_values = plot_data$n, metric_values = plot_data$AUC)
    stab_val <- bp$bp
    
    stab_bin <- if (!is.na(stab_val)) {
      bins <- as.numeric(levels(plot_data$n_bin))
      bins[which.min(abs(bins - stab_val))]
    } else NA
    
    p <- ggplot(plot_data, aes(x = n_bin, y = AUC)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5) +
      stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred") +
      theme_bw() +
      ylim(0, 1) +
      theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 7),
            axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
            axis.text.y = element_text(size = 6),
            panel.grid.minor = element_blank()) +
      labs(x = "n", y = "AUC", title = quality_label)
    
    if (!is.na(stab_bin)) {
      p <- p +
        geom_vline(xintercept = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                   color = "firebrick", linetype = "dashed") +
        annotate("text", x = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                 y = min(plot_data$AUC, na.rm = TRUE),
                 label = paste0("n=", round(stab_val)),
                 angle = 90, vjust = 1.5, size = 2.5, color = "darkred")
    }
    
    list(plot = p, eval = eval_label, quality = quality_label, stab_val = stab_val)
  })
})

# flatten nested list
all_results_flat <- unlist(all_results, recursive = FALSE)

# split plots and thresholds
all_plots <- lapply(all_results_flat, function(x) x$plot)

threshold_df <- tibble::tibble(eval = sapply(all_results_flat, function(x) x$eval),
                               quality  = sapply(all_results_flat, function(x) x$quality),
                               stab_val = sapply(all_results_flat, function(x) x$stab_val))

write.csv(threshold_df, file = paste0(envrmt$path_evaluation, "/Plots/All_AUC_Thresholds_by_FitQuality.csv"),
          row.names = FALSE)

# helper function to create a rotated row label
row_label <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label,
             angle = 90, size = 4, fontface = "bold") +
    theme_void()
}

# plots for pa, po random and po balanced
row_PA <- wrap_plots(row_label("PA"),
                     wrap_plots(all_plots[1:3], ncol = 3),
                     ncol = 2, widths = c(0.01, 1))

row_PO_Rand <- wrap_plots(row_label("PO-Random"),
                          wrap_plots(all_plots[4:6], ncol = 3),
                          ncol = 2, widths = c(0.01, 1))

row_PO_Bal <- wrap_plots(row_label("PO-Balanced"),
                         wrap_plots(all_plots[7:9], ncol = 3),
                         ncol = 2, widths = c(0.01, 1))

combined_plot <- row_PA / row_PO_Bal / row_PO_Rand

ggsave(filename = paste0(envrmt$path_evaluation, "/Plots/All_AUC_by_FitQuality.png"), plot = combined_plot,
       width = 17, height = 14, dpi = 300)

# ================================================================
# 6. AUC threshold by sampling strategy
# ================================================================

# sampling strategy grouping
strat_lookup <- c("Random" = "Random",
                  "Cluster" = "Cluster",
                  "Block" = "Block",
                  "Convenience" = "Convenience",
                  "Systematic" = "Systematic",
                  "Snowball" = "Snowball",
                  "Leave-Out" = "LeaveOut",
                  "Stratified" = "Stratified",
                  "Effort-Driven" = "EffortDriven",
                  "Preferential" = "Preferential")

for (eval in c("PA", "PO_Random", "PO_Balanced")) {

#for (eval in c("PA")) {  
  data_path <- paste0(envrmt$path_evaluation, "/", eval, "/")
  strat_df <- list.files(data_path, full.names = TRUE) %>% map_df(readRDS)
  
  # format and filter to n 1-300
  strat_df <- strat_df %>%
    dplyr::mutate(n = as.numeric(as.character(n))) %>%
    dplyr::filter(n >= 1, n <= 300, !is.na(AUC)) %>%
    dplyr::mutate(
      n_bin_val = cut(n, breaks = seq(0, max(n, na.rm = TRUE) + bin_size, by = bin_size),
                      labels = FALSE) * bin_size,
      n_bin = factor(n_bin_val),
      # recode strat column to display names
      strat = factor(names(strat_lookup)[match(strat, strat_lookup)], levels = names(strat_lookup)))
  
  # one plot per strategy
  strat_results <- lapply(levels(strat_df$strat), function(strategy) {
    
    plot_data <- strat_df %>% dplyr::filter(strat == strategy)
    
    # get breakpoint
    bp <- get_breakpoint(n_values = plot_data$n, metric_values = plot_data$AUC)
    stab_val <- bp$bp
    #print("Calculated thresholds")
    
    stab_bin <- if (!is.na(stab_val)) {
      bins <- as.numeric(levels(plot_data$n_bin))
      bins[which.min(abs(bins - stab_val))]
    } else NA
    
    p <- ggplot(plot_data, aes(x = n_bin, y = AUC)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5) +
      stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred") +
      theme_bw() +
      ylim(0, 1) +
      theme(
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 7),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
        axis.text.y = element_text(size = 6),
        panel.grid.minor = element_blank()
      ) + labs(x = "n", y = "AUC", title = strategy)
    
    
    #print("Plotted the boxplots")
    if (!is.na(stab_bin)) {
      p <- p +
        geom_vline(xintercept = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                   color = "firebrick", linetype = "dashed") +
        annotate("text", x = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                 y = min(plot_data$AUC, na.rm = TRUE), label = paste0("n=", round(stab_val)),
                 angle = 90, vjust = 1.5, size = 2.5, color = "darkred")
    }
    
    list(plot = p, strat = strategy, stab_val = stab_val)
  })
  
  #print("Annotated the line")
  
  # split plots and thresholds
  strat_plots <- lapply(strat_results, function(x) x$plot)
  
  threshold_df <- tibble::tibble(eval = eval, strat = sapply(strat_results, function(x) x$strat),
                                 stab_val = sapply(strat_results, function(x) x$stab_val))
  
  write.csv(threshold_df, file = paste0(envrmt$path_evaluation, "/Plots/", eval, "_AUC_Thresholds_by_Strat.csv"),
            row.names = FALSE)
  
  #print("Wrote the csv")
  
  # 2 rows of 5 for 10 strategies
  combined_plot <- wrap_plots(strat_plots, ncol = 5, nrow = 2)
  
  ggsave(filename = paste0(envrmt$path_evaluation, "/Plots/", eval, "_AUC_by_Strat.png"), 
         plot = combined_plot, width = 20, height = 10, dpi = 300)
}

