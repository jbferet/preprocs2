#' get asset from planetary using gdal
#'
#' @param item character.
#' @param asset_names character.
#' @param aoi sf.
#' @param crs_target numeric.
#' @param collection character.
#' @param resampling character.
#' @param resolution numeric.
#'
#' @return features
#' @importFrom terra same.crs crs crop project
#' @importFrom sf st_transform
#' @importFrom rstac items_sign sign_planetary_computer assets_url
#' @importFrom stats setNames
#' @export
#'
get_asset_terra <- function(item, asset_names, aoi, crs_target = NULL,
                            collection = 'sentinel-2-l2a', 
                            resampling = 'near', resolution = 10){

  if (collection =='sentinel-2-l2a')
    features <- item |>
      # sign again if the signed url has expired
      rstac::items_sign(
        rstac::sign_planetary_computer()) |>
      # get the urls adding the vsicurl prefix for GDAL
      assets_url(asset_names = asset_names, append_gdalvsi = TRUE) |>
      lapply(rast) |>
      setNames(asset_names)
  if (collection %in% c('sentinel2-l2a-sen2lasrc',
                        'sentinel2-l2a-theia',
                        'sentinel2-l2a-sen2cor'))
    features <- item |>
      # sign again if the signed url has expired
      rstactheia::items_sign_theia() |>
      # get the urls adding the vsicurl prefix for GDAL
      assets_url(asset_names = asset_names, append_gdalvsi = TRUE) |>
      lapply(rast) |>
      setNames(asset_names)

  if (! terra::same.crs(x = aoi, y = features[[1]]))
    aoi <- sf::st_transform(x = aoi, crs = terra::crs(features[[1]]))
  features <- suppressWarnings(lapply(features, terra::crop,
                                      y = aoi, mask = TRUE))
  if (!is.null(crs_target))
    features <- lapply(X = features, FUN = terra::project,
                       y = paste0('epsg:',crs_target), res = resolution,
                       method = resampling)
  return(features)
}
