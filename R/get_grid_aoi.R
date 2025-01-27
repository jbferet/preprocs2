#' return vector grid corresponding to an area of interest
#'
#' @param aoi_path character. path for vector file defining aoi
#' @param cellsize numeric. square cell size in meters
#' @param crs_target numeric. CRS corresponding to the output grid cell
#' @param dsn_grid character. path corresponding to grid
#' @param dsn_centroid character. path corresponding to centroids of grid
#'
#' @return dsn_grid path for grid of aoi
#' @importFrom sf read_sf st_transform st_make_grid st_intersects st_buffer st_write st_centroid
#' @importFrom crsuggest suggest_crs
#' @export

get_grid_aoi <- function(aoi_path, cellsize = 10000, crs_target = 4326,
                         dsn_grid = NULL, dsn_centroid = NULL){
  aoi <- sf::read_sf(aoi_path, quiet = T)
  # transform vector into crs applicable for metric system
  crs_current <- crsuggest::suggest_crs(input = aoi)[1,]
  aoi_grid <- sf::st_transform(aoi, crs=crs_current$crs_proj4) |>
    st_make_grid(cellsize = cellsize, square = TRUE)
  # select grid celles intersecting with initial aoi
  intersect <- as.data.frame(st_intersects(x = aoi_grid,
                                           st_transform(aoi, crs=crs_current$crs_proj4)))
  aoi_grid <- aoi_grid[intersect$row.id]
  # add buffer to avoid border effects
  aoi_grid <- sf::st_buffer(x = aoi_grid, dist = 100)
  # transform vector into crs_target
  aoi_grid_init <- sf::st_transform(aoi_grid, crs=crs_target)
  data_path <- dirname(aoi_path)
  # save grid
  if (is.null(dsn_grid))
    dsn_grid <- file.path(data_path,'aoi_grid.gpkg')
  sf::st_write(obj = aoi_grid_init, dsn = dsn_grid,
               overwrite = T, append = F,
               driver = 'GPKG', quiet = T)
  # save grid centroid for each cell
  if (is.null(dsn_centroid))
    dsn_centroid <- file.path(data_path,'aoi_grid_centroid.gpkg')
  aoi_grid_centroids <- sf::st_centroid(x = aoi_grid_init)
  sf::st_write(obj = aoi_grid_centroids, dsn = dsn_centroid,
               overwrite = T, append = F,
               driver = 'GPKG', quiet = T)
  return(list('dsn_grid' = dsn_grid,
              'dsn_centroid' = dsn_centroid))
}
