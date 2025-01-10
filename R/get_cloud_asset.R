#' get name for cloud asset in item collection
#'
#' @param item_collection item collection
#' @param collection character. S2 collection
#'
#' @return asset_names corresponding to cloud or scene classification
#' @importFrom stringr str_detect
#' @export
#'
get_cloud_asset <- function(item_collection, collection){
  href <- item_collection$links[[2]]$href
  if (stringr::str_detect(href, pattern = 'planetary'))
    asset_names <- 'SCL'
  if (stringr::str_detect(href, pattern = 'cdos') & collection == 'sentinel2-l2a-sen2lasrc')
    asset_names <- 'CLM'
  return(asset_names)
}
