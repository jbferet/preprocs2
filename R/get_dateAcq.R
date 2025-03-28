#' gets acquisition date from planetary URL
#'
#' @param band_url character.
#' @param S2product character.
#' @param collection character.
#'
#' @return DateAcq dates of acquisition
#' @importFrom stringr str_split
#' @export
#'
get_dateAcq <- function (band_url = NULL, S2product = NULL, collection = 'sentinel-2-l2a') {
  if (! is.null(band_url)){
    band_url <- basename(band_url)
    if (collection == 'sentinel-2-l2a'){
      band_url2 <- lapply(stringr::str_split(string = band_url, pattern = "_"),'[[',2)
    } else if (collection == 'sentinel2-l2a-sen2lasrc'){
      band_url2 <- lapply(stringr::str_split(string = band_url, pattern = "_"),'[[',3)
    }
  } else if (! is.null(S2product)){
    S2product <- basename(S2product)
    band_url2 <- lapply(stringr::str_split(string = S2product, pattern = "_"),'[[',3)
  }
  band_url_acq <- lapply(stringr::str_split(string = band_url2, pattern = "T"),'[[',1)
  DateAcq <- as.Date(unlist(band_url_acq), format = "%Y%m%d")
  return(DateAcq)
}
