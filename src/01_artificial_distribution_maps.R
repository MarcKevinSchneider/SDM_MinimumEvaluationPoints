#' @name 01_artificial_distribution_maps.R
#' @date 08.12.2025
#' @author Marc Kevin Schneider & Lisa Bald
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Creates the artificial distribution maps used in this study using the 
#' distribution of the ten virtual species from Grimmet et al. 2020. 
#' 
#' Artificial distribution maps are applied by using the true presence-absence
#' distribution of a virtual species and applying a Gaussian random field using
#' a predetermined level of autocorrelation.
#' 
#' Original code by Lisa Bald (contact: bald@staff.uni-marburg.de). Adjusted by
#' Marc Kevin Schneider.

# ================================================================
# 1. Load setup script
# ================================================================
rootDir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Masterarbeit/Minimum_Evaluation_Points_SDM/"
# calling the setup script
path <- file.path(rootDir, "src", "00_setup_project.R")
source(path, echo = TRUE)

# ================================================================
# 2. Configure RandomFields settings
# ================================================================
RandomFields::RFoptions(cPrintlevel = 0,
                        spConform = FALSE,
                        install = "no",
                        seed = NULL)

# ================================================================
# 3. Calculate artificial distribution maps
# ================================================================

set.seed(2962)

# the 10 VS from Grimmet et al. 2020
species <-  c("VS01", "VS02", "VS03", "VS04", 
            "VS05", "VS06", "VS07", "VS08", "VS09", "VS10")

# goodness of fit between the "real" distribution of the VS and the artificial
# distribution maps
goodnessOfFit <- seq(0, 1, 0.1)

# for every species
for (vs in species){
  # load real distribution
  realDistribution <- terra::rast(paste0(envrmt$path_paRaster, "/", 
                                         as.character(vs),".tif"))
  # if directory of artificial distribution maps doesnt exist for this species
  # then create one
  if(!dir.exists(paste0(envrmt$path_ADM, as.character(vs)))){
    dir.create(paste0(envrmt$path_ADM, "/", as.character(vs)), recursive = TRUE)
  }
  # for every fit level
  for (fit in goodnessOfFit){
    # check if file exists, if it doesnt then execute code
    if(!file.exists(paste0(envrmt$path_ADM, "/", as.character(vs), "/",
                           as.character(vs), "_", "Fit_", as.character(fit),
                           ".tif"))){
      # simulate Gaussian random field (spatial autocorrelation range sampled randomly)
      autocorrRange=sample(c(20,50,100,500,800),size=1)
      
      # same size and resolution as original distribution
      randomField=nlm_gaussianfield(nrow=1777, ncol=2247, resolution=813.3488,
                                    autocorr_range = autocorrRange)
      # make it a raster
      randomField=terra::rast(randomField)
      # fit to same extent and crs as original distribution
      terra::ext(randomField) <- terra::ext(realDistribution)
      terra::crs(randomField)<- terra::crs(realDistribution)
      randomField=terra::mask(randomField, realDistribution)
      
      # combine random field with true distribution (weighted mixture)
      randomEffects <- fit
      x1 <-( randomEffects * realDistribution)
      x2 <- ((1-randomEffects) * randomField)
      pred <- x1 + x2
      trueCor <- terra::layerCor(terra::rast(list(pred,realDistribution)),
                                 fun="cor")$correlation[[1,2]]
      
      # if random effects are not 0, then execute the code
      if(randomEffects != 0){
        if (round(abs(trueCor-randomEffects),2) >= 0.02){
          x=1
          y=1
          # scale artificial distribution map if correlation is too high
          # gradually increases the influence of the random effects while
          # decreasing the influence of the true distribution
          if(trueCor > randomEffects){
            while(round(abs(trueCor-randomEffects),2) >= 0.02){
              x1 <-( randomEffects * realDistribution *y)
              x2 <- ((1-randomEffects) * randomField *x)
              pred <- x1 + x2
              trueCor <- terra::layerCor(terra::rast(list(pred,realDistribution)),
                                         fun="cor")$correlation[[1,2]]
              x <- x+0.01
              y <- y-0.01
              print(trueCor)
              if(trueCor < 0) break
            }
          }
          # scale artificial distribution map if correlation is too low
          # gradually decreases the influence of the random effects while
          # increasing the influence of the true distribution
          if(trueCor < randomEffects){
            while(round(abs(trueCor-randomEffects),2) <= 0.02){
              x1 <-( randomEffects * realDistribution *y)
              x2 <- ((1-randomEffects) * randomField *x)
              pred <- x1 + x2
              trueCor <- terra::layerCor(terra::rast(list(pred,realDistribution)),
                                         fun="cor")$correlation[[1,2]]
              x <- x-0.01
              y <- y+0.01
              print(trueCor)
              if(trueCor < 0) break
            }
          }
        }
      }
      print(paste("Random effect is:",randomEffects,". Correlation is:", round(trueCor,2)))
      
      # normalize the data to between 0 and 1 to match 
      # the probability of occurrence from SDM models
      pred=climateStability::rescale0to1(pred)
      
      # save raster 
      terra::writeRaster(pred, paste0(envrmt$path_ADM, "/", as.character(vs), "/",
                                      as.character(vs), "_", "Fit_", as.character(fit),
                                      ".tif"))
      # or if file exists just read the data
    } else pred=terra::rast(paste0(envrmt$path_ADM, "/", as.character(vs), "/",
                                   as.character(vs), "_", "Fit_", as.character(fit),
                                   ".tif"))
  }
}





