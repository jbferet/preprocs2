#' This function adjusts information from ENVI header
#'
#' @param dsn character. path where to store the stack
#' @param Bands list. should include 'bandname', and if possible 'wavelength'
#' @param sensor character. Name of the sensor used to acquire the image
#' @param Stretch boolean. Set TRUE to get 10% stretching at display for reflectance, mentioned in hdr only
#'
#' @return None
#' @export

adjust_ENVI_hdr <- function(dsn, Bands, sensor = 'Unknown', Stretch = FALSE){

  # Edit HDR file to add metadata
  HDR <- read_ENVI_header(get_HDR_name(dsn))
  HDR$`band names` <- Bands$bandname
  if (length(Bands$wavelength)==length(Bands$bandname)){
    HDR$wavelength <- Bands$wavelength
    if (!is.null(Bands$`wavelength units`)){
      HDR$`wavelength units` <- 'nanometers'
    }
  } else {
    HDR$wavelength <- NULL
  }
  if (Stretch==TRUE) HDR$`default stretch` <- '0.000000 1000.000000 linear'
  HDR$`z plot range` <- NULL
  HDR$`data ignore value` <- '-Inf'
  HDR$`sensor type` <- sensor
  write_ENVI_header(HDR = HDR,HDRpath = get_HDR_name(dsn))

  # remove unnecessary files
  File2Remove <- paste(dsn, ".aux.xml", sep = "")
  if (file.exists(File2Remove)) file.remove(File2Remove)
  File2Remove <- paste(dsn, ".prj", sep = "")
  if (file.exists(File2Remove)) file.remove(File2Remove)
  File2Remove <- paste(dsn, ".stx", sep = "")
  if (file.exists(File2Remove)) file.remove(File2Remove)
  return(invisible())
}

