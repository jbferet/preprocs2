#' produce S2 stack and correct it if needed (BRDF or B2)
#' These are very experimental corrections
#'
#' @param s2_items path for spectral bands required
#' @param acq date of acquisition
#' @param raster_dir directory where rasters are stored
#' @param aoi sf.
#' @param offset_B2 boolean. value of the offset to apply to the S2 band B2
#' @param corr_BRF boolean.
#' @param bands_to_correct character. name of bands to correct from geometry
#'
#' @return list of collections per plot
#' @importFrom terra rast writeRaster values
#' @export
#'
correct_s2_stack <- function(s2_items, acq, raster_dir, aoi,
                            offset_B2 = F, corr_BRF = F,
                            bands_to_correct = c('B8A', 'B11', 'B12')){
  # correct for geometry of acquisition: get correcting factors
  output_dir <- dirname(raster_dir)
  if (corr_BRF)
    s2_items <- correct_geom(S2_rast = s2_items, aoi = aoi,
                             output_dir = output_dir, acq = acq,
                             bands_to_correct = bands_to_correct)
  # harmonize bands with uniform B2 value (experimental)
  if (offset_B2){
    B2 <- s2_items$B02
    offB2 <- 300-B2
    offB2[B2>500] <- 0
    for (bb in seq_along(s2_items))
      s2_items[[bb]] <- s2_items[[bb]] + offB2
  }
  s2_items <- terra::rast(s2_items)
  return(s2_items)
}
