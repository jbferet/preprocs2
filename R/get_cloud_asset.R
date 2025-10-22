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
  if (stringr::str_detect(href, pattern = 'planetary')){
    asset_names <- 'SCL'
  } else if (stringr::str_detect(href, pattern = 'api.stac.teledetection')){
    if (collection == 'sentinel2-l2a-sen2lasrc')
      asset_names <- 'CLM'
    if (collection == 'sentinel2-l2a-theia')
      asset_names <- 'CLM_R1'
    if (collection == 'sentinel2-l2a-sen2cor')
      asset_names <- 'SCL'
  } else {
    message('STAC URL unidentified from get_cloud_asset')
    message('assuming standard oESA products and looking for SCL')
    asset_names <- 'SCL'
  }
  return(asset_names)
}
