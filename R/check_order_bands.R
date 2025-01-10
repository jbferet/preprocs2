#' checks band order
#'
#' @param acq character.
#' @param patterns character.
#'
#' @return acq updated acquisitions
#' @importFrom stringr str_detect
#' @export
#'
check_order_bands <- function(acq, patterns){
  ord <- c()
  for (pat in patterns) ord <- c(ord, which(stringr::str_detect(string = acq, pattern = pat)))
  acq <- acq[ord]
  return(acq)
}
