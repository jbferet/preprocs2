#' get asset from planetary using gdal
#'
#' @param asset_url character.
#' @param out_file character.
#' @param plots_bbox sf..
#'
#' @return vsicurl url
#' @importFrom sf st_crs st_bbox gdal_utils
#' @export
#'
get_asset <- function(asset_url, out_file, plots_bbox) {
  sf::gdal_utils(
    "warp",
    source = asset_url,
    destination = out_file,
    options = c(
      "-t_srs", sf::st_crs(plots_bbox)$wkt,
      "-te", sf::st_bbox(plots_bbox)
    )
  )
  return(invisible())
}
