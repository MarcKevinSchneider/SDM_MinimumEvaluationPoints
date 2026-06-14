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
rootDir <- "/home/Marc/SDM_MinimumEvaluationPoints"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# set seed
set.seed(2962)

# sourcing the evaluation functions
source(paste0(envrmt$path_src, "/functions/evaluation_functions.R"))

# ================================================================
# 2. Presence-Absence evaluation
# ================================================================

# number of cores for parallelization
nCores <- 48

results <- mclapply(
  X = seq_len(nrow(params)),
  FUN = function(i) {
    
    # 1. Extract the parameters
    #--------------------------------------------------------
    sp <- params$sp[i]
    fit <- params$fit[i]
    n <- params$n[i]
    iter <- params$iter[i]
    strat <- params$strat[i]
    
    tryCatch(
      {
        # 2. check if file already exists, skip if it does
        #--------------------------------------------------------
        dir_pres  <- paste0(envrmt$path_PresenceAbsence, "/", strat, "/", sp, "/", n)
        rds_path  <- paste0(dir_pres, "/", sp, "_Fit_", fit, "_Iteration_", iter, "_Pres_Abs.rds")
        
        if (file.exists(rds_path)) {
          #print(paste0("Skipping (already exists): Strategy: ", strat,
          #             ", Species: ", sp, ", Sample Size: ", n,
          #             ", Fit: ", fit, ", Iteration: ", iter))
          return(NULL)
        }
        
        # 3. Load presence-absence data
        #--------------------------------------------------------
        file_path <- paste0(envrmt$path_pre_abs_points, "/", strat, "/",
                            sp, "/", n, "/", sp, "_Fit_",
                            fit, "_Iteration_", iter, "_Pres_Abs.gpkg")
        
        if (!file.exists(file_path)) return(NULL)
        pres_abs <- sf::read_sf(file_path)
        
        # 4. Load background data
        #--------------------------------------------------------
        bck_path <- paste0(envrmt$path_bkg_points, "/Random/",
                           sp, "/", sp, "_Fit_",
                           fit, "_Background.gpkg")
        
        if (!file.exists(bck_path)) return(NULL)
        bck_pts <- sf::read_sf(bck_path)
        
        # 5. Load artificial distribution map data
        #--------------------------------------------------------
        ls_path <- paste0(envrmt$path_ADM, "/", sp, "/",
                          sp, "_Fit_", fit, ".tif")
        
        if (!file.exists(ls_path)) return(NULL)
        adm <- terra::rast(ls_path)
        
        # 6. Load original "true" presence-absence raster data
        #--------------------------------------------------------
        pa_path <- paste0(envrmt$path_paRaster, "/", sp, ".tif")
        
        if (!file.exists(pa_path)) return(NULL)
        paRaster <- terra::rast(pa_path)
        
        # 7. Optimal threshold selection
        #--------------------------------------------------------
        presences <- pres_abs
        
        # now we convert the ADM layer for each presence absence dataset to a "prediction"
        # from https://gitup.uni-potsdam.de/macroecology/mecofun/-/blob/master/R/evalSDM.R?ref_type=heads
        # uses the code from parts of the evalSDM function
        thresh.dat <- data.frame(ID = seq_len(length(presences$Observed)),
                                 obs = presences$Observed,
                                 pred = presences$lyr.1)
        
        # threshold methods
        thresh.mat <- PresenceAbsence::optimal.thresholds(DATA = thresh.dat,
                                                          req.sens = 0.85,
                                                          req.spec = 0.85,
                                                          FPC = 1,
                                                          FNC = 1)
        
        # we use maximized sensitivity and specificity here
        thresh <- thresh.mat[thresh.mat$Method == "MaxSens+Spec", 2]
        
        # binary classification
        presences$Predicted <- ifelse(presences$lyr.1 >= thresh, 1, 0)
        
        # calculate the evaluation emtrics
        metrics <- eval_funcs(presences)
        
        #print(paste0("Finished evaluation for Sampling Strategy: ", strat,
        #             ", Species: ", sp, ", Sample Size: ", n,
        #             ", Fit: ", fit, ", Iteration: ", iter))
        
        # to temporary dataframe
        tmp_df <- data.frame(
          strat  = strat,
          sp     = sp,
          n      = n,
          fit    = fit,
          iter   = iter,
          thresh = thresh,
          AUC    = metrics$AUC,
          MAE    = metrics$MAE,
          RMSE   = metrics$RMSE,
          TSS    = metrics$TSS,
          COR    = metrics$COR,
          JAC    = metrics$JAC,
          DIS    = metrics$DIS,
          SOR    = metrics$SOR
        )
        
        # 6. Save RDS 
        #--------------------------------------------------------
        if (!dir.exists(dir_pres)) dir.create(dir_pres, recursive = TRUE)
        saveRDS(tmp_df, rds_path)
        
        # return NULL on success
        NULL  
      },
      error = function(e) {
        data.frame(
          strat         = strat,
          species       = sp,
          fit           = fit,
          n             = n,
          iteration     = iter,
          error_message = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }
    )
  },
  mc.cores = nCores
)


# combine the fail logs from the workers
failed_runs <- do.call(rbind, results)

# save if there are failed runs
if (!is.null(failed_runs)) {
  write.csv(failed_runs, paste0(envrmt$path_docs, "/Evaluation_failures_09032026.csv"),
            row.names = FALSE)}


# ================================================================
# 3. Presence-Only evaluation
# ================================================================

results <- mclapply(
  X = seq_len(nrow(params)),
  FUN = function(i) {
    
    # 1. Extract the parameters
    #--------------------------------------------------------
    sp <- params$sp[i]
    fit <- params$fit[i]
    n <- params$n[i]
    iter <- params$iter[i]
    strat <- params$strat[i]
    
    tryCatch(
      {
        # 2. Check if BOTH background output RDS files already exist, if so, skip entirely
        #--------------------------------------------------------
        dir_pres_random <- paste0(envrmt$path_RandomBKG, "/", strat, "/", sp, "/", n)
        dir_pres_balanced <- paste0(envrmt$path_BalancedBKG, "/", strat, "/", sp, "/", n)
        rds_po_bal_path <- paste0(dir_pres_balanced, "/Balanced_", sp, "_Fit_", fit, "_Iteration_", iter, "_Pres_Only.rds")
        rds_po_ran_path <- paste0(dir_pres_random, "/Random_", sp, "_Fit_", fit, "_Iteration_", iter, "_Pres_Only.rds")
        
        if (file.exists(rds_po_bal_path) && file.exists(rds_po_ran_path)) {
          #print(paste0("Skipping (already exists): Strategy: ", strat,
          #             ", Species: ", sp, ", Sample Size: ", n,
          #             ", Fit: ", fit, ", Iteration: ", iter))
          return(NULL)
        }
        
        # 3. Load presence-absence data
        #--------------------------------------------------------
        file_path <- paste0(envrmt$path_pre_abs_points, "/", strat, "/",
                            sp, "/", n, "/", sp, "_Fit_",
                            fit, "_Iteration_", iter, "_Pres_Abs.gpkg")
        
        if (!file.exists(file_path)) return(NULL)
        pres_abs <- sf::read_sf(file_path)
        
        # 4. Load random background data
        #--------------------------------------------------------
        random_bck_path <- paste0(envrmt$path_bkg_points, "/Random/",
                           sp, "/", sp, "_Fit_",
                           fit, "_Background.gpkg")
        
        if (!file.exists(random_bck_path)) return(NULL)
        random_bck_pts <- sf::read_sf(random_bck_path)
        
        # 3. Load balanced background data
        #--------------------------------------------------------
        balanced_bck_path <- paste0(envrmt$path_bkg_points, "/Balanced/",
                           sp, "/", n, "/", sp, "_Fit_",
                           fit, "_Iteration_", iter, "_Background.gpkg")
        
        if (!file.exists(balanced_bck_path)) return(NULL)
        balanced_bck_pts <- sf::read_sf(balanced_bck_path)
        
        
        # 5. Load artificial distribution map data
        #--------------------------------------------------------
        ls_path <- paste0(envrmt$path_ADM, "/", sp, "/",
                          sp, "_Fit_", fit, ".tif")
        
        if (!file.exists(ls_path)) return(NULL)
        adm <- terra::rast(ls_path)
        
        # 6. Load original "true" presence-absence raster data
        #--------------------------------------------------------
        pa_path <- paste0(envrmt$path_paRaster, "/", sp, ".tif")
        
        if (!file.exists(pa_path)) return(NULL)
        paRaster <- terra::rast(pa_path)
        
        
        # 7. Presence-Only evaluation with random background points
        #--------------------------------------------------------
        if (!file.exists(rds_po_ran_path)) {
          
          # assigns Observed = 1 to presences, Observed = 0 to background
          # this treats background points as pseudo absences
          pres_only <- pres_abs[pres_abs$Observed == 1, c("Observed", "lyr.1")]
          bck_pseudo <- random_bck_pts[, c("lyr.1")]
          bck_pseudo$Observed <- 0L
          
          # combine into single sf object
          po_combined <- rbind(pres_only, bck_pseudo[, c("Observed", "lyr.1")])
          
          
          # now we convert the ADM layer for each presence dataset to a "prediction"
          # from https://gitup.uni-potsdam.de/macroecology/mecofun/-/blob/master/R/evalSDM.R?ref_type=heads
          # uses the code from parts of the evalSDM function
          thresh.dat.po <- data.frame(ID = seq_len(nrow(po_combined)),
                                      obs = po_combined$Observed,
                                      pred = po_combined$lyr.1)
          
          # threshold methods
          thresh.mat.po <- PresenceAbsence::optimal.thresholds(DATA = thresh.dat.po,
                                                               req.sens = 0.85,
                                                               req.spec = 0.85,
                                                               FPC = 1,
                                                               FNC = 1)
          # we use maximized sensitivity and specificity here
          thresh.po <- thresh.mat.po[thresh.mat.po$Method == "MaxSens+Spec", 2]
          
          # binary classification
          po_combined$Predicted <- ifelse(po_combined$lyr.1 >= thresh.po, 1, 0)
          
          # calculate the evaluation emtrics
          metrics.po <- eval_funcs(po_combined)
          
          # to temporary dataframe
          random_tmp_df_po <- data.frame(
            strat  = strat,
            sp     = sp,
            n      = n,
            fit    = fit,
            iter   = iter,
            thresh = thresh.po,
            AUC    = metrics.po$AUC,
            MAE    = metrics.po$MAE,
            RMSE   = metrics.po$RMSE,
            TSS    = metrics.po$TSS,
            COR    = metrics.po$COR,
            JAC    = metrics.po$JAC,
            DIS    = metrics.po$DIS,
            SOR    = metrics.po$SOR
          )
          
          # save RDS
          if (!dir.exists(dir_pres_random)) dir.create(dir_pres_random, recursive = TRUE)
          saveRDS(random_tmp_df_po, rds_po_ran_path)
          
          print(paste0("Finished PO evaluation — Strategy: ", strat,
                       ", Species: ", sp, ", Sample Size: ", n,
                       ", Fit: ", fit, ", Iteration: ", iter))
        }
        
        # 8. Presence-Only evaluation with balanced background points
        #--------------------------------------------------------
        if (!file.exists(rds_po_bal_path)) {
          
          # assigns Observed = 1 to presences, Observed = 0 to background
          # this treats background points as pseudo absences
          pres_only <- pres_abs[pres_abs$Observed == 1, c("Observed", "lyr.1")]
          bck_pseudo <- balanced_bck_pts[, c("lyr.1")]
          bck_pseudo$Observed <- 0L
          
          # combine into single sf object
          po_combined <- rbind(pres_only, bck_pseudo[, c("Observed", "lyr.1")])
          
          # now we convert the ADM layer for each presence dataset to a "prediction"
          # from https://gitup.uni-potsdam.de/macroecology/mecofun/-/blob/master/R/evalSDM.R?ref_type=heads
          # uses the code from parts of the evalSDM function
          thresh.dat.po <- data.frame(ID = seq_len(nrow(po_combined)),
                                      obs = po_combined$Observed,
                                      pred = po_combined$lyr.1)
          
          # threshold methods
          thresh.mat.po <- PresenceAbsence::optimal.thresholds(DATA = thresh.dat.po,
                                                               req.sens = 0.85,
                                                               req.spec = 0.85,
                                                               FPC = 1,
                                                               FNC = 1)
          
          # we use maximized sensitivity and specificity here
          thresh.po <- thresh.mat.po[thresh.mat.po$Method == "MaxSens+Spec", 2]
          
          # binary classification
          po_combined$Predicted <- ifelse(po_combined$lyr.1 >= thresh.po, 1, 0)
          
          # calculate evaluation emtrics
          metrics.po <- eval_funcs(po_combined)
          
          # to temporary dataframe
          balanced_tmp_df_po <- data.frame(
            strat  = strat,
            sp     = sp,
            n      = n,
            fit    = fit,
            iter   = iter,
            thresh = thresh.po,
            AUC    = metrics.po$AUC,
            MAE    = metrics.po$MAE,
            RMSE   = metrics.po$RMSE,
            TSS    = metrics.po$TSS,
            COR    = metrics.po$COR,
            JAC    = metrics.po$JAC,
            DIS    = metrics.po$DIS,
            SOR    = metrics.po$SOR
          )
          
          # save RDS
          if (!dir.exists(dir_pres_balanced)) dir.create(dir_pres_balanced, recursive = TRUE)
          saveRDS(balanced_tmp_df_po, rds_po_bal_path)
          
          #print(paste0("Finished PO evaluation — Strategy: ", strat,
          #             ", Species: ", sp, ", Sample Size: ", n,
          #             ", Fit: ", fit, ", Iteration: ", iter))
        }
        
        # return NULL on success
        NULL
      },
      error = function(e) {
        data.frame(
          strat         = strat,
          species       = sp,
          fit           = fit,
          n             = n,
          iteration     = iter,
          error_message = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }
    )
  },
  mc.cores = nCores
)

# combine the fail logs from the workers
failed_runs <- do.call(rbind, results)

# save if there are failed runs
if (!is.null(failed_runs)) {
  write.csv(failed_runs, paste0(envrmt$path_docs, "/Background_Evaluation_failures_10032026.csv"),
            row.names = FALSE)
}



# ================================================================
# 4. Combine Presence-Absence RDS files into csv
# ================================================================

# presence absence files
pa_files <- list.files(path = envrmt$path_PresenceAbsence, pattern = "_Pres_Abs\\.rds$",
                       recursive = TRUE, full.names = TRUE)

# read and combine
pa_combined <- do.call(rbind, lapply(pa_files, function(f) {
  tryCatch(readRDS(f), error = function(e) {
    message("Failed to read: ", f, " — ", conditionMessage(e))
    NULL
  })
}))

# save RDS and csv
saveRDS(pa_combined, paste0(envrmt$path_docs, "/PA_Evaluation_combined.rds"))
write.csv(pa_combined, paste0(envrmt$path_docs, "/PA_Evaluation_combined.csv"), row.names = FALSE)



# ================================================================
# 5. Combine Presence-Only with random background points RDS files into csv
# ================================================================

# presence only with random background files
po_ran_files <- list.files(path = envrmt$path_RandomBKG, pattern = "^Random_.*_Pres_Only\\.rds$",
                           recursive = TRUE, full.names = TRUE)

# read and combine
po_ran_combined <- do.call(rbind, lapply(po_ran_files, function(f) {
  tryCatch(readRDS(f), error = function(e) {
    message("Failed to read: ", f, " — ", conditionMessage(e))
    NULL
  })
}))

# save RDS and csv
saveRDS(po_ran_combined, paste0(envrmt$path_docs, "/PO_Random_Evaluation_combined.rds"))
write.csv(po_ran_combined, paste0(envrmt$path_docs, "/PO_Random_Evaluation_combined.csv"), row.names = FALSE)


# ================================================================
# 5. Combine Presence-Only with balanced background points RDS files into csv
# ================================================================

# presence only with balanced background files
po_bal_files <- list.files(path = envrmt$path_BalancedBKG, pattern = "^Balanced_.*_Pres_Only\\.rds$",
                           recursive = TRUE, full.names = TRUE)

# read and combine
po_bal_combined <- do.call(rbind, lapply(po_bal_files, function(f) {
  tryCatch(readRDS(f), error = function(e) {
    message("Failed to read: ", f, " — ", conditionMessage(e))
    NULL
  })
}))

# save RDS and csv
saveRDS(po_bal_combined, paste0(envrmt$path_docs, "/PO_Balanced_Evaluation_combined.rds"))
write.csv(po_bal_combined, paste0(envrmt$path_docs, "/PO_Balanced_Evaluation_combined.csv"), row.names = FALSE)
