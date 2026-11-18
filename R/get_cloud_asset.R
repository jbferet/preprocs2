#' get name for cloud asset in item collection
#'
#' @param stac_info list.
#'
#' @return asset_names corresponding to cloud or scene classification
#' @importFrom stringr str_detect
#' @export
#'
get_cloud_asset <- function(stac_info){
  if (stac_info$provider =='mpc'){
    asset_names <- 'SCL'
  } else if (stac_info$provider =='lasrc'){
    asset_names <- 'CLM'
  } else if (stac_info$provider =='theia' & stac_info$collection =='sentinel2-l2a-theia'){
    asset_names <- 'CLM_R1'
  } else if (stac_info$provider =='theia' & stac_info$collection =='sentinel2-l2a-sen2cor'){
    asset_names <- 'SCL'
  } else if (stac_info$provider %in% c('mtd_esa')){
    asset_names <- 'SCL'
  } else {
    message('STAC URL unidentified from get_cloud_asset')
    message('assuming standard ESA products and looking for SCL')
    asset_names <- 'SCL'
  }
  return(asset_names)
}
