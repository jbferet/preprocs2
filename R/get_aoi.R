#' check if S2 tiling grid is available
#'
#' @param aoi_path character. path for aoi
#' @param crs_target numeric.
#'
#' @return valid path for S2 tiling grid
#' @importFrom crsuggest suggest_crs
#' @importFrom sf st_transform
#' @export

get_aoi <- function(aoi_path, crs_target = 4326){
  aoi <- sf::read_sf(aoi_path)
  # transform vector into crs applicable for metric system
  crs_current <- crsuggest::suggest_crs(input = aoi)[1,]
  aoi_transform <- sf::st_transform(aoi, crs=crs_current$crs_proj4)
  # select grid celles intersecting with initial aoi
  # transform vector into crs_target
  aoi <- sf::st_transform(aoi_transform, crs=crs_target)
  aoi <- list('001' =  aoi)
  return(aoi)
}