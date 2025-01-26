#' return footprint corresponding to an vector file
#'
#' @param dsn character path for vector file
#'
#' @return footprint
#' @importFrom sf st_read st_crs st_bbox
#' @export

get_s2_footprint <- function(dsn){
  plots_sf <- sf::st_read(dsn = dsn, quiet = T)[[1]]
  if (inherits(x = plots_sf, what = 'character'))
    plots_sf <- sf::st_read(dsn = dsn, quiet = T)
  crs_plots <- sf::st_crs(plots_sf)
  footprint <- bbox_to_poly(x = sf::st_bbox(plots_sf), crs = crs_plots)
  return(footprint)
}
