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
# 3. Overview plot over all strats, fits and virtual species
# ================================================================

# read PA data
pa_path <- paste0(envrmt$path_evaluation, "/PA/")
pa_df <- list.files(pa_path, full.names = TRUE) %>% map_df(readRDS)


# relative stability for coefficient of variation
bin_size        <- 10
ref_n           <- 300

# Threshold for improvement
# stops when CV shrinks by less than this amount
improvement_threshold <- 0.001

metrics <- c("AUC", "MAE", "RMSE", "TSS", "COR", "JAC", "DIS", "SOR")

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
  
  # 1. Calculate coefficient of variation per n
  #--------------------------------------------------------
  summary_df <- plot_data %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(
      mean_val = mean(.data[[metric]], na.rm = TRUE),
      sd_val   = sd(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # cv calc by dividing the sd by the mean
    dplyr::mutate(cv_val = ifelse(mean_val == 0, 0, sd_val / abs(mean_val))) %>%
    dplyr::arrange(n)
  
  # 2. Smoothing the curve
  #--------------------------------------------------------
  stable_df <- summary_df %>%
    dplyr::mutate(
      # smooth by 20 values
      cv_val_smoothed = zoo::rollmean(cv_val, k = 20, fill = NA, align = "right")
    ) %>%
    dplyr::mutate(
      # now calculate the difference based on the smoothed trend
      cv_diff = abs(dplyr::lag(cv_val_smoothed) - cv_val_smoothed),
      stable_step = cv_diff <= improvement_threshold,
      stays_stable = rev(cumall(rev(ifelse(is.na(stable_step), TRUE, stable_step))))
    )

  # calc the value at whichs the prediction is stable
  stab_val <- stable_df %>% 
    dplyr::filter(stays_stable & n > bin_size) %>%
    dplyr::slice(1) %>% 
    dplyr::pull(n)

  
  # mapping the stability value onto the bins
  # so tha tthe plot is correctly displayed
  stab_bin <- if (!is.na(stab_val)) {
    bins <- as.numeric(levels(plot_data$n_bin))
    bins[which.min(abs(bins - stab_val))]
  } else NA
  
  
  # 3. Actual plot now
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
  
    #labs(title = metric, x = "n", y = metric)
  
  if (!is.na(stab_bin)) {
    p <- p +
      geom_vline(xintercept = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
                 color = "firebrick", linetype = "dashed") +
      annotate("text", x = as.numeric(factor(stab_bin, levels = levels(plot_data$n_bin))),
               y = min(plot_data[[metric]], na.rm = TRUE),
               label = paste0("n=", stab_val), angle = 90, vjust = 1.5, size = 2.5, color= "darkred")
  }
  return(p)
})

# combine and Save
combined_plot <- wrap_plots(metric_plots, ncol = 4) 

ggsave(
  filename = paste0(envrmt$path_evaluation, "/Plots/All_Metrics_CV_Stability.png"),
  plot     = combined_plot,
  width    = 16, height = 8, dpi = 300
)


# ================================================================
# 4. Same plot but simplified
# ================================================================

# pivot to long format
pa_df_long <- pa_df %>%
  tidyr::pivot_longer(
    cols = all_of(metrics),
    names_to  = "metric",
    values_to = "value"
  )

# pre-compute stability thresholds per metric
stab_vals <- setNames(
  lapply(metrics, function(metric) {
    plot_data <- pa_df %>% dplyr::filter(!is.na(.data[[metric]]))
    
    # calc cv again
    summary_df <- plot_data %>%
      dplyr::group_by(n) %>%
      dplyr::summarise(
        mean_val = mean(.data[[metric]], na.rm = TRUE),
        sd_val   = sd(.data[[metric]],   na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(cv_val = ifelse(mean_val == 0, 0, sd_val / abs(mean_val))) %>%
      dplyr::arrange(n)
    
    # same as before
    stable_df <- summary_df %>%
      dplyr::mutate(
        cv_val_smoothed = zoo::rollmean(cv_val, k = 20, fill = NA, align = "right"),
        cv_diff         = abs(dplyr::lag(cv_val_smoothed) - cv_val_smoothed),
        stable_step     = cv_diff <= improvement_threshold,
        stays_stable    = rev(cumall(rev(ifelse(is.na(stable_step), TRUE, stable_step))))
      )
    
    stab_val <- stable_df %>%
      dplyr::filter(stays_stable & n > bin_size) %>%
      dplyr::slice(1) %>%
      dplyr::pull(n)
    
    if (length(stab_val) == 0) NA else stab_val
  }),
  metrics
)

# cvonert to tidy df so I can use it for plotting later
stab_df <- tibble::tibble(
  metric   = names(stab_vals),
  stab_val = unlist(stab_vals)
) %>% dplyr::filter(!is.na(stab_val))

# summarise long data
summary_long <- pa_df_long %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::group_by(metric, n) %>%
  dplyr::summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val   = sd(value,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  dplyr::left_join(stab_df, by = "metric") %>%
  dplyr::mutate(metric = factor(metric, levels = metrics))

# clip sd values to between 1 and -1
# otherwise the sd will go above or below that in the plot
summary_long <- summary_long %>%
  dplyr::mutate(
    ymin_ribbon = dplyr::case_when(
      metric == "COR" ~ pmax(mean_val - sd_val, -1),
      TRUE            ~ pmax(mean_val - sd_val,  0)
    ),
    ymax_ribbon = pmin(mean_val + sd_val, 1)
  )

# plotting 
p <- ggplot(summary_long, aes(x = n, y = mean_val)) +
  geom_ribbon(aes(ymin = ymin_ribbon, ymax = ymax_ribbon),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_vline(data = stab_df %>% dplyr::mutate(metric = factor(metric, levels = metrics)),
             aes(xintercept = stab_val),
             color = "firebrick", linetype = "dashed", linewidth = 0.6) +
  geom_text(data = stab_df %>% dplyr::mutate(metric = factor(metric, levels = metrics)),
            aes(x = stab_val, y = -Inf, label = paste0("n=", stab_val)),
            angle = 90, vjust = -0.5, hjust = -1, color = "firebrick", size = 4.5) +
  facet_wrap(~ metric, scales = "fixed", ncol = 4) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.text       = element_text(face = "bold", size = 9),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y      = element_text(size = 7),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  ) +
  labs(x = "Number of presence records (n)", y= "Mean value", size= 8)

ggsave(
  filename = paste0(envrmt$path_evaluation, "/Plots/Simplified_Metrics_GraphicalAbstract.png"),
  plot     = p,
  width    = 16, height = 8, dpi = 300
)

# ================================================================
# 5. Further simplified for graphical abstract
# ================================================================

# only AUC and TSS for this plot
focal_metrics <- c("AUC", "TSS")
metric_colors <- c("AUC" = "steelblue", "TSS" = "darkorange")

# pre-compute stability thresholds for AUC and tss
stab_vals_focal <- setNames(
  lapply(focal_metrics, function(metric) {
    plot_data <- pa_df %>% dplyr::filter(!is.na(.data[[metric]]))
    
    # calc cv again
    summary_df <- plot_data %>%
      dplyr::group_by(n) %>%
      dplyr::summarise(
        mean_val = mean(.data[[metric]], na.rm = TRUE),
        sd_val   = sd(.data[[metric]],   na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(cv_val = ifelse(mean_val == 0, 0, sd_val / abs(mean_val))) %>%
      dplyr::arrange(n)
    
    # same as before
    stable_df <- summary_df %>%
      dplyr::mutate(
        cv_val_smoothed = zoo::rollmean(cv_val, k = 20, fill = NA, align = "right"),
        cv_diff         = abs(dplyr::lag(cv_val_smoothed) - cv_val_smoothed),
        stable_step     = cv_diff <= improvement_threshold,
        stays_stable    = rev(cumall(rev(ifelse(is.na(stable_step), TRUE, stable_step))))
      )
    
    stab_val <- stable_df %>%
      dplyr::filter(stays_stable & n > bin_size) %>%
      dplyr::slice(1) %>%
      dplyr::pull(n)
    
    if (length(stab_val) == 0) NA else stab_val
  }),
  focal_metrics
)

# cvonert to tidy df so I can use it for plotting later
stab_df_focal <- tibble::tibble(
  metric   = names(stab_vals_focal),
  stab_val = unlist(stab_vals_focal)
) %>% dplyr::filter(!is.na(stab_val))

# summarise long data for auc and tss only
summary_focal <- pa_df %>%
  tidyr::pivot_longer(cols = all_of(focal_metrics),
                      names_to = "metric", values_to = "value") %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::group_by(metric, n) %>%
  dplyr::summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val   = sd(value,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(metric = factor(metric, levels = focal_metrics))

# plotting
p_focal <- ggplot(summary_focal, aes(x = n, y = mean_val,
                                     color = metric, fill = metric)) +
  geom_ribbon(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.0) +
  geom_vline(data = stab_df_focal,
             aes(xintercept = stab_val, color = metric),
             linetype = "dashed", linewidth = 0.7) +
  geom_text(data = stab_df_focal,
            aes(x = stab_val, y = -Inf,
                label = paste0("n=", stab_val), color = metric),
            angle = 90, vjust = -0.5, hjust = -1, size = 6.5) +
  scale_color_manual(values = metric_colors) +
  scale_fill_manual(values  = metric_colors) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  ) +
  labs(
    x = "Number of presence records (n)",
    y = "Model performance"
  )

ggsave(
  filename = paste0(envrmt$path_evaluation, "/Plots/Simplified_AUC_TSS_GraphicalAbstract.png"),
  plot     = p_focal,
  width    = 12, height = 8, dpi = 300
)


# ================================================================
# 6. Threshold identification using exponential curve fitting
# ================================================================

get_breakpoint <- function(n_values, metric_values, pct = 0.99) {
  
  # 1. Aggregate to median and IQR per n
  #--------------------------------------------------------
  summary_data <- data.frame(n = n_values, metric = metric_values) %>%
    dplyr::mutate(n = as.numeric(as.character(n))) %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(
      med_val = median(metric, na.rm = TRUE),
      q25     = quantile(metric, 0.25, na.rm = TRUE),
      q75     = quantile(metric, 0.75, na.rm = TRUE),
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
    exp_mod <- nls(med_val ~ a * exp(b * n) + c, data    = summary_data,
      start   = start_vals, control = nls.control(maxiter = 500, tol = 1e-4))
    
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


# read PA data
pa_path <- paste0(envrmt$path_evaluation, "/PA/")
#pa_path <- paste0(envrmt$path_evaluation, "/PO_Random/")
#pa_path <- paste0(envrmt$path_evaluation, "/PO_Balanced/")
pa_df <- list.files(pa_path, full.names = TRUE) %>% map_df(readRDS)

# pivot to long format
pa_df_long <- pa_df %>%
  tidyr::pivot_longer(
    cols = all_of(metrics),
    names_to  = "metric",
    values_to = "value"
  )

# pre compute breakpoints and summaries per metric
bp_results <- setNames(
  lapply(metrics, function(metric) {
    plot_data <- pa_df %>% dplyr::filter(!is.na(.data[[metric]]))
    get_breakpoint(n_values = plot_data$n, metric_values = plot_data[[metric]])
  }),
  metrics
)

# extract breakpoints into tidy df for plotting
stab_df <- tibble::tibble(
  metric   = names(bp_results),
  stab_val = sapply(bp_results, function(x) x$bp)
) %>% dplyr::filter(!is.na(stab_val))

# extract and combine per-metric summaries (median + IQR) into long df
summary_long <- dplyr::bind_rows(
  lapply(metrics, function(metric) {
    bp_results[[metric]]$summary %>%
      dplyr::mutate(metric = metric)
  })
) %>%
  dplyr::left_join(stab_df, by = "metric") %>%
  dplyr::mutate(metric = factor(metric, levels = metrics)) %>%
  # clip IQR to valid range per metric
  dplyr::mutate(
    ymin_ribbon = dplyr::case_when(
      metric == "COR" ~ pmax(q25, -1),
      TRUE            ~ pmax(q25,  0)
    ),
    ymax_ribbon = pmin(q75, 1)
  )

# plotting
p <- ggplot(summary_long, aes(x = n, y = med_val)) +
  geom_ribbon(aes(ymin = ymin_ribbon, ymax = ymax_ribbon),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_vline(data = stab_df %>% dplyr::mutate(metric = factor(metric, levels = metrics)),
             aes(xintercept = stab_val),
             color = "firebrick", linetype = "dashed", linewidth = 0.6) +
  geom_text(data = stab_df %>% dplyr::mutate(metric = factor(metric, levels = metrics)),
            aes(x = stab_val, y = -Inf, label = paste0("n=", round(stab_val))),
            angle = 90, vjust = -0.5, hjust = -1, color = "firebrick", size = 4.5) +
  facet_wrap(~ metric, scales = "fixed", ncol = 4) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.text       = element_text(face = "bold", size = 9),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y      = element_text(size = 7),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  ) +
  labs(x = "Number of presence records (n)", y = "Median value")

ggsave(
  filename = paste0(envrmt$path_evaluation, "/Plots/PA_Simplified_Metrics_FittedCurve.png"),
  plot     = p,
  width    = 16, height = 8, dpi = 300
)

