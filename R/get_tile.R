#' This function gets tile from S2 image
#'
#' @param prodName character. original name for the S2 image
#' @param stac_info which stac endpoint?
#'
#' @return TileName character
#' @importFrom tools file_path_sans_ext
#' @export

get_tile <- function(prodName, stac_info){
  prodName <- basename(prodName)
  if (stac_info$provider %in% c('theia') & stac_info$collection == 'sentinel2-l2a-sen2cor'){
    TileName <- unlist(lapply(strsplit(split = "_", x = prodName), '[[', 3))
  } else {
    TileName <- tools::file_path_sans_ext(gsub("_.*", "", gsub(".*_T", "", prodName)))
  }
  return(TileName)
}

