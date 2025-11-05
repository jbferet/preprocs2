#' updates vegetation mask based on initial SCL and readiometric thresholds
#'
#' @param raster_path character. path for raster
#' @param wavelength numeric. wavelengths corresponding to spectral bands. ENVI HDR expected if NULL
#' @param driver character. driver / extension for raster
#' @param fact numeric. aggregation factor
#' @param normalize boolean. should normalization be applied?
#'
#' @return none
#' @importFrom terra rast writeRaster values
#' @export
#'

get_quicklook <- function(raster_path, wavelength = NULL, driver = 'png',
                          fact = NULL, normalize = T){
  img <- terra::rast(raster_path)
  if (is.null(wavelength)){
    hdr <- read_envi_header(get_HDR_name(raster_path))
    wavelength <- hdr$wavelength
  }
  lambda <- list('R' = 690, 'G' = 590, 'B' = 490)
  selband <- c()
  for (l in lambda)
    selband <- c(selband, which(abs(wavelength-l)== min(abs(wavelength-l))))

  img_rgb <- img[[selband]]
  img_rgb_path <- paste0(tools::file_path_sans_ext(raster_path), '_RBG.',driver)
  if (!is.null(fact))
    img_rgb <- terra::aggregate(x = img_rgb, fact = fact)

  if (normalize){
    terra::values(img_rgb)[which(terra::values(img_rgb)<0)] <- 0
    terra::values(img_rgb)[which(terra::values(img_rgb)>3000)] <- 3000
    img_rgb <- img_rgb*3000/10000
  }
  terra::writeRaster(x = img_rgb, filename = img_rgb_path, overwrite = T)
  return()
}
