#' gets processing baseline from S2 SAFE product 
#'
#' @param stac_info list.
#' @param item_collection path for collection to download
#'
#' @return baseline
#' @export
#'
get_s2_baseline <- function(stac_info, item_collection){
  if (stac_info$provider %in% c('mpc', 'esa', 'mtd_esa')){
    baseline <- lapply(lapply(item_collection$features,'[[','properties'),
                       '[[', 's2:processing_baseline')
  } else if (stac_info$provider == 'sen2lasrc'){
    baseline <- lapply(lapply(lapply(item_collection$features,'[[','properties'),
                              '[[', 'processing:software'),
                       '[[', 'sen2lasrc')
  } else {
    baseline <- NULL
  }
  return(baseline)
}
