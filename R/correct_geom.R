#' corrects geometry of acquisition
#'
#' @param S2_rast S2 spatRaster
#' @param output_dir character.
#' @param aoi sf object
#' @param acq date of acquisition
#'
#' @return list of collections per plot
#' @importFrom terra rast values na.omit crop buffer
#' @import prosail
#' @importFrom methods as
#' @export
#'
correct_geom <- function(S2_rast, output_dir, aoi, acq){

  geom_dir <- file.path(output_dir, 'geomAcq_S2')
  # download geometry of acquisition corresponding to aoi
  angle <- c('saa', 'sza', 'vaa', 'vza')
  mean_angle <- list()
  for (jj in 1:4){
    fileName <- file.path(geom_dir, paste0(angle[jj], '_', as.character(acq), '.tiff'))
    geom <- terra::crop(x = terra::rast(fileName), y = as(aoi, "Spatial"))
    if (!length(terra::na.omit(terra::values(geom)))==0){
      geom <- terra::project(x = geom, y = S2_rast[[1]])
    } else {
      width <- 200
      while (length(na.omit(terra::values(geom)))==0){
        geom <- terra::crop(x = terra::rast(fileName),
                            y = terra::buffer(x = as(aoi, "Spatial"),
                                              width = width), snap = 'out')
        width <- width + 200
      }
    }
    mean_angle[[angle[jj]]] <- base::mean(terra::values(geom),na.rm = T)
  }
  # produce prosail LUT corresponding to mean geometry of acquisition
  GeomAcq <- list()
  psi <- abs(mean_angle$saa-mean_angle$vaa)
  if (psi>180) psi <- 360-psi
  GeomAcq$min <- data.frame('tto' = mean_angle$vza, 'tts' = mean_angle$sza, 'psi' = psi)
  GeomAcq$max <- data.frame('tto' = mean_angle$vza, 'tts' = mean_angle$sza, 'psi' = psi)
  # get prosail simulations for current acquisition and for standard configuration
  nbsamples <- 100
  SensorName <- 'Sentinel_2'
  SRF <- prosail::GetRadiometry(SensorName)
  InputPROSAIL <- prosail::get_InputPROSAIL(atbd = TRUE, nbSamples = nbsamples,
                                            GeomAcq = GeomAcq)
  res <- Generate_LUT_PROSAIL(SAILversion = '4SAIL',
                              InputPROSAIL = InputPROSAIL,
                              SpecPROSPECT = prospect::SpecPROSPECT_FullRange,
                              SpecSOIL = prosail::SpecSOIL,
                              SpecATM = prosail::SpecATM)
  BRF_LUT_1nm <- res$BRF
  BRF_LUT <- applySensorCharacteristics(wvl = prospect::SpecPROSPECT_FullRange$lambda,
                                        SRF = SRF, InRefl = BRF_LUT_1nm)

  # produce prosail LUT in standard nadir conditions
  InputPROSAIL_standard <- InputPROSAIL
  InputPROSAIL_standard$tto <- 0
  InputPROSAIL_standard$tts <- 45
  InputPROSAIL_standard$psi <- 90
  res <- Generate_LUT_PROSAIL(SAILversion = '4SAIL',
                              InputPROSAIL = InputPROSAIL_standard,
                              SpecPROSPECT = prospect::SpecPROSPECT_FullRange,
                              SpecSOIL = prosail::SpecSOIL,
                              SpecATM = prosail::SpecATM)
  BRF_LUT_1nm <- res$BRF
  BRF_LUT_standard <- applySensorCharacteristics(wvl = prospect::SpecPROSPECT_FullRange$lambda,
                                                 SRF = SRF,
                                                 InRefl = BRF_LUT_1nm)
  BRF_LUT <- t(as.matrix(BRF_LUT))
  BRF_LUT_standard <- t(as.matrix(BRF_LUT_standard))
  colnames(BRF_LUT) <- colnames(BRF_LUT_standard) <- SRF$Spectral_Bands
  ratioBRF <- 1+(BRF_LUT_standard-BRF_LUT)/BRF_LUT
  ratioCorr <- colMeans(ratioBRF)
  names(S2_rast) <- SRF$Spectral_Bands
  bands2correct <- c('B8A', 'B11', 'B12')
  for (band in bands2correct){
    S2_rast[[band]] <- ratioCorr[band] * S2_rast[[band]]
    terra::values(S2_rast[[band]]) <- round(terra::values(S2_rast[[band]]))
  }
  return(S2_rast)
}
