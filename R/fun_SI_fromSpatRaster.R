#' compute spectral indices and composite for a given period of time
#'
#' @param S2_refl spatRaster.
#' @param S2_mask spatRaster.
#' @param argsin list of input arguments
#'
#' @return resSI
#' @importFrom terra rast values writeRaster median
#' @importFrom stringr str_replace
#' @export
#'
fun_SI_fromSpatRaster <- function(S2_refl, S2_mask, argsin){

  ReflFactor <- argsin$ReflFactor
  SI_list <- argsin$SI_list
  output_path <- argsin$output_path
  site_name <- argsin$site_name
  # define parameters related to Sentinel-2
  HDRpath <- system.file("extdata", "SENTINEL_2.hdr", package = "preprocS2")
  HDR <- read_envi_header(HDRpath = HDRpath)
  # get SI for each S2 acquisition
  SI_val <- lapply(X = S2_refl,
                   FUN = spinR::compute_S2SI_Raster,
                   ReflFactor = ReflFactor, StackOut = FALSE,
                   SensorBands = HDR$wavelength, Sel_Indices = SI_list)

  # mask unwanted pixels
  SI_masked <- list()
  for (idx in SI_list)
    SI_masked[[idx]] <- list()
  for (doa in names(SI_val)){
    SI_val[[doa]]$listIndices <- NULL
    elim <- which(terra::values(S2_mask[[doa]])==0 | is.na(terra::values(S2_mask[[doa]])))
    for (idx in SI_list){
      SI_tmp <- SI_val[[doa]]$SpectralIndices[[idx]]
      if (length(elim)>0){
        terra::values(SI_tmp)[elim] <- NA
      }
      SI_masked[[idx]][[doa]] <- SI_tmp
      if (argsin$write_SI_acq){
        filename <- file.path(argsin$path_SI_acq,
                              paste0(idx,'_',argsin$iChar,'_',doa,'.tiff'))
        terra::writeRaster(x = SI_masked[[idx]][[doa]], filename = filename,
                           filetype = 'GTiff', overwrite = argsin$overwrite)
      }
    }
  }
  sirast <- sirast_median <- list()
  for (idx in SI_list){
    sirast[[idx]] <- terra::rast(SI_masked[[idx]])
    sirast_median[[idx]] <- terra::median(sirast[[idx]], na.rm = TRUE)
    # file name
    filename <- stringr::str_replace(string = argsin$template_file,
                                     pattern = '_ID_',
                                     replacement = paste0('_', argsin$iChar,'_'))
    filename <- stringr::str_replace(string = filename,
                                     pattern = '_SI.',
                                     replacement = paste0('_', idx,'.'))
    filename <- stringr::str_replace(string = filename,
                                     pattern = 'plot_',
                                     replacement = paste0(site_name, '_'))
    dir.create(path = output_path, showWarnings = FALSE, recursive = TRUE)
    # write composites
    filename <- file.path(output_path, filename)
    if (argsin$overwrite | ! file.exists(filename))
      terra::writeRaster(x = sirast_median[[idx]], filename = filename,
                         datatype = argsin$datatype,
                         overwrite = argsin$overwrite)
  }
  resSI <- list('individual_SI' = SI_val, 'synthesis_SI' = sirast_median)
  return(resSI)
}
