#' corrects geometry of acquisition
#'
#' @param S2_rast S2 spatRaster
#' @param output_dir character.
#' @param aoi sf object
#' @param acq date of acquisition
#' @param bands_to_correct character. name of bands to correct from geometry
#'
#' @return list of collections per plot
#' @importFrom terra rast values na.omit crop buffer project
#' @import prosail
#' @importFrom methods as
#' @importFrom utils packageVersion
#' @export
#'
correct_geom <- function(S2_rast, output_dir, aoi, acq,
                         bands_to_correct = c('B8A', 'B11', 'B12')){

  geom_dir <- file.path(output_dir, 'geom_acq_S2')
  # download geometry of acquisition corresponding to aoi
  angle <- c('saa', 'sza', 'vaa', 'vza')
  mean_angle <- list()
  for (jj in 1:4){
    fileName <- file.path(geom_dir, paste0(angle[jj], '_',
                                           as.character(acq), '.tiff'))
    anglerast <- terra::rast(fileName)
    if (!terra::same.crs(anglerast, aoi))
      aoi <- sf::st_transform(x = aoi, terra::crs(anglerast))
    geom <- terra::crop(x = anglerast, y = aoi)
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
  geom_acq <- list()
  psi <- abs(mean_angle$saa-mean_angle$vaa)
  if (psi>180)
    psi <- 360-psi
  geom_acq$min <- data.frame('tto' = mean_angle$vza,
                             'tts' = mean_angle$sza,
                             'psi' = psi)
  geom_acq$max <- data.frame('tto' = mean_angle$vza,
                             'tts' = mean_angle$sza,
                             'psi' = psi)
  # get prosail simulations for current acquisition and for standard configuration
  nbsamples <- 100
  SensorName <- 'Sentinel_2'
  if (utils::packageVersion("prosail")<'3.0.0'){
    lambda <- prosail::SpecPROSPECT_FullRange$lambda
    srf <- prosail::GetRadiometry(SensorName)
    InputPROSAIL <- prosail::get_InputPROSAIL(atbd = TRUE, nbSamples = nbsamples,
                                              geom_acq = geom_acq)
    res <- Generate_LUT_PROSAIL(SAILversion = '4SAIL',
                                InputPROSAIL = InputPROSAIL,
                                SpecPROSPECT = prosail::SpecPROSPECT_FullRange,
                                SpecSOIL = prosail::SpecSOIL,
                                SpecATM = prosail::SpecATM)
    refl_LUT_1nm <- res$BRF
    refl_LUT <- applySensorCharacteristics(wvl = lambda,
                                          SRF = srf, InRefl = refl_LUT_1nm)

    # produce prosail LUT in standard nadir conditions
    InputPROSAIL_standard <- InputPROSAIL
    InputPROSAIL_standard$tto <- 0
    InputPROSAIL_standard$tts <- 45
    InputPROSAIL_standard$psi <- 90
    res <- Generate_LUT_PROSAIL(SAILversion = '4SAIL',
                                InputPROSAIL = InputPROSAIL_standard,
                                SpecPROSPECT = prosail::SpecPROSPECT_FullRange,
                                SpecSOIL = prosail::SpecSOIL,
                                SpecATM = prosail::SpecATM)
    refl_LUT_1nm <- res$BRF
    refl_LUT_ref <- applySensorCharacteristics(wvl = lambda, SRF = srf,
                                                   InRefl = refl_LUT_1nm)
  } else {
    lambda <- prosail::spec_prospect_full_range$lambda
    srf <- prosail::get_srf_sensor(sensor_name = SensorName)
    InputPROSAIL <- prosail::get_input_prosail(atbd = TRUE,
                                               nb_samples = nbsamples,
                                               geom_acq = geom_acq)
    res <- generate_lut_prosail(SAILversion = '4SAIL',
                                input_prosail = InputPROSAIL,
                                spec_prospect = prosail::spec_prospect_full_range,
                                spec_soil = prosail::spec_soil_atbd_v2,
                                spec_atm = prosail::spec_atm)
    refl_LUT_1nm <- res$surf_refl
    refl_LUT <- apply_sensor_characteristics(wvl = lambda, srf = srf,
                                            refl = refl_LUT_1nm)

    # produce prosail LUT in standard nadir conditions
    InputPROSAIL_standard <- InputPROSAIL
    InputPROSAIL_standard$tto <- 0
    InputPROSAIL_standard$tts <- 45
    InputPROSAIL_standard$psi <- 90
    res <- generate_lut_prosail(SAILversion = '4SAIL',
                                input_prosail = InputPROSAIL_standard,
                                spec_prospect = prosail::spec_prospect_full_range,
                                spec_soil = prosail::spec_soil_atbd_v2,
                                spec_atm = prosail::spec_atm)
    refl_LUT_1nm <- res$surf_refl
    refl_LUT_ref <- apply_sensor_characteristics(wvl = lambda, srf = srf,
                                                refl = refl_LUT_1nm)

  }
  refl_LUT <- t(as.matrix(refl_LUT))
  refl_LUT_ref <- t(as.matrix(refl_LUT_ref))
  colnames(refl_LUT) <- colnames(refl_LUT_ref) <- srf$Spectral_Bands
  ratioBRF <- 1+(refl_LUT_ref-refl_LUT)/refl_LUT
  ratioCorr <- colMeans(ratioBRF)
  originalnames <- names(S2_rast)
  names(S2_rast) <- srf$Spectral_Bands
  # bands_to_correct <- c('B8A', 'B11', 'B12')
  # bands_to_correct <- names(S2_rast)

  for (band in bands_to_correct){
    S2_rast[[band]] <- ratioCorr[band] * S2_rast[[band]]
    terra::values(S2_rast[[band]]) <- round(terra::values(S2_rast[[band]]))
  }
  names(S2_rast) <- originalnames
  return(S2_rast)
}
