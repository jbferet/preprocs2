#' produce S2 stack and correct it if needed (BRDF or B2)
#'
#' @param s2_items path for spectral bands required
#' @param acq date of acquisition
#' @param raster_dir directory where rasters are stored
#' @param baseline processing baseeline for acquisition to account for offset
#' @param template_Rast spatRaster template raster
#' @param asset_names character.
#' @param iChar plot ID
#' @param aoi sf.
#' @param offset value of the offset to apply to the S2 bands
#' @param offset_B2 boolean. value of the offset to apply to the S2 band B2
#' @param corr_BRF boolean.
#' @param clean_bands boolean.
#'
#' @return list of collections per plot
#' @importFrom terra rast writeRaster values
#' @export
#'
correct_s2stack <- function(s2_items, acq, raster_dir, baseline, template_Rast,
                            asset_names, iChar, aoi, offset = 1000,
                            offset_B2 = F, corr_BRF = F, clean_bands = T){
  S2_rast <- lapply(X = s2_items, FUN = terra::rast)
  if (!is.null(template_Rast))
    S2_rast  <- lapply(X = S2_rast, FUN = terra::resample, template_Rast, method = 'near')
  # correct for geometry of acquisition: get correcting factors
  output_dir <- dirname(raster_dir)
  if (corr_BRF)
    S2_rast <- correct_geom(S2_rast = S2_rast, aoi = aoi, output_dir = output_dir, acq = acq)
  if (offset_B2){
    B2 <- S2_rast[[1]]
    if (as.numeric(baseline)>=4) terra::values(B2) <- terra::values(B2)-offset
    offB2 <- 300-B2
    offB2[B2>500] <- 0
    for (bb in seq_len(length(S2_rast))) S2_rast[[bb]] <- S2_rast[[bb]] + offB2
  }
  S2_rast  <- terra::rast(S2_rast)
  names(S2_rast) <- asset_names
  # correct offset
  if (!is.na(as.numeric(baseline))){
    if (as.numeric(baseline)>=4)
      terra::values(S2_rast) <- terra::values(S2_rast)-offset
  } 
  out_S2_file <- file.path(raster_dir,paste('plot_',iChar,'_',acq, '.tiff', sep = ''))
  terra::writeRaster(x = S2_rast, filename = out_S2_file, overwrite = T)
  if (clean_bands) unlink(x = s2_items)
  return()
}
