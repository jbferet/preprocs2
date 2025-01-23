#' This function saves reflectance files
#'
#' @param S2Sat character. Sentinel-2 mission ('2A' or '2B')
#' @param tile_S2 character. S2 tile name (2 numbers + 3 letters)
#' @param dateAcq_S2 double. date of acquisition
#'
#' @return s2mission character. name of the S2 mission (2A or 2B)
#' @export

check_S2mission <- function(S2Sat = NULL, tile_S2, dateAcq_S2){
  
  # is mission already defined by user?
  if (!is.null(S2Sat)){
    if (S2Sat %in% c('2A', '2B')){
      s2mission <- S2Sat
    } else {
      message('Could not identify if image from Sentinel-2A or -2B')
      message('Defining central wavelength of spectral bands based on S2A')
      s2mission <- '2A'
    }
  } else {
    message('Could not identify if image from Sentinel-2A or -2B')
    message('Defining central wavelength of spectral bands based on S2A')
    s2mission <- '2A'
  }
  return(s2mission)
}