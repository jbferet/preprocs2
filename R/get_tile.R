#' This function gets tile from S2 image
#'
#' @param prodName character. original name for the S2 image
#'
#' @return TileName character
#' @importFrom tools file_path_sans_ext
#' @export

get_tile <- function(prodName){
  prodName <- basename(prodName)
  TileName <- tools::file_path_sans_ext(gsub("_.*", "", gsub(".*_T", "", prodName)))
  return(TileName)
}

