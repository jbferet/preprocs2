#' This function adjusts information from ENVI header
#'
#' @param dsn character. path where to store the stack
#' @param Bands list. should include 'bandname', and if possible 'wavelength'
#' @param sensor character. Name of the sensor used to acquire the image
#' @param Stretch boolean. Set TRUE to get 10% stretching at display for reflectance, mentioned in hdr only
#'
#' @return None
#' @export

adjust_envi_hdr <- function(dsn, Bands, sensor = 'Unknown', Stretch = FALSE){

  # Edit HDR file to add metadata
  hdr <- read_envi_header(hdr_path = get_hdr_name(image_path = dsn))
  hdr$`band names` <- Bands$bandname
  if (length(Bands$wavelength)==length(Bands$bandname)){
    hdr$wavelength <- Bands$wavelength
    if (!is.null(Bands$`wavelength units`))
      hdr$`wavelength units` <- 'nanometers'
  } else {
    hdr$wavelength <- NULL
  }
  if (Stretch==TRUE)
    hdr$`default stretch` <- '0.000000 1000.000000 linear'
  hdr$`z plot range` <- NULL
  hdr$`data ignore value` <- '-Inf'
  hdr$`sensor type` <- sensor
  write_envi_header(hdr = hdr,
                    hdr_path = get_hdr_name(image_path = dsn))

  # remove unnecessary files
  file_to_remove <- paste(dsn, ".aux.xml", sep = "")
  if (file.exists(file_to_remove))
    file.remove(file_to_remove)
  file_to_remove <- paste(dsn, ".prj", sep = "")
  if (file.exists(file_to_remove))
    file.remove(file_to_remove)
  file_to_remove <- paste(dsn, ".stx", sep = "")
  if (file.exists(file_to_remove))
    file.remove(file_to_remove)
  return(invisible())
}

