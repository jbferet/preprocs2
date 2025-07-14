#' return vector grid corresponding to an area of interest
#'
#' @param aoi_path character. path for vector file defining aoi
#' @param cellsize numeric. square cell size in meters
#' @param output_dir character. output path
#' @param crs_target crs to be applied to data collected
#'
#' @return dsn_grid path for grid of aoi
#' @importFrom sf read_sf st_transform st_make_grid st_intersects st_buffer st_write st_centroid sf_use_s2
#' @importFrom crsuggest suggest_crs suggest_top_crs
#' @export

get_grid_aoi <- function(aoi_path, cellsize = 10000, output_dir, crs_target = NULL){

  # define grid
  dsn_grid <- file.path(output_dir, paste0('aoi_grid_',cellsize,'.gpkg'))
  # read aoi and get crs corresponding to S2 acquisitions
  if (file.exists(dsn_grid)){
    message('Reading grid')
    aoi_grid <- sf::st_read(dsn = dsn_grid, quiet = T)
    if (is.null(crs_target))
      crs_target <- suppressMessages(crsuggest::suggest_top_crs(input = aoi_grid,
                                                                units = 'm'))
    nbcells <- length(aoi_grid$geom)
  } else {
    message('Reading aoi')
    aoi <- sf::read_sf(aoi_path, quiet = T)
    if (is.null(crs_target))
      crs_target <- suppressMessages(crsuggest::suggest_top_crs(input = aoi,
                                                                units = 'm'))
    aoi <- sf::st_transform(aoi, crs = crs_target)

    # convert cellsize in degrees if meters and required by CRS
    if (sf::st_crs(aoi)$units_gdal=='degree' & cellsize > 10){
      suppressMessages(sf::sf_use_s2(FALSE))
      suppressWarnings(centroid_aoi <- sf::st_centroid(x = sf::st_sf(aoi)))
      suppressMessages(sf::sf_use_s2(TRUE))
      occs_df <- data.frame('latitude' = centroid_aoi$geom[[1]][2],
                            'longitude' = centroid_aoi$geom[[1]][1],
                            'distance' = cellsize)
      distlatlon <- meters_to_decdeg(occs_df = occs_df,
                                     lat_col = 'latitude',
                                     lon_col = 'longitude',
                                     distance = 'distance')
      cellsize <- mean(unlist(distlatlon))
    }
    # transform vector into crs applicable for metric system if needed
    aoi_grid <- aoi |> sf::st_make_grid(cellsize = cellsize, square = TRUE)
    # select grid cells intersecting with initial aoi
    suppressMessages(sf::sf_use_s2(FALSE))
    intersect <- as.data.frame(sf::st_intersects(x = aoi_grid, aoi))
    suppressMessages(sf::sf_use_s2(TRUE))
    aoi_grid <- aoi_grid[intersect$row.id]
    # add buffer to avoid border effects
    aoi_grid <- sf::st_buffer(x = aoi_grid, dist = 100)
    # save grid
    dir.create(path = output_dir, showWarnings = F, recursive = T)
    sf::st_write(obj = aoi_grid, dsn = dsn_grid,
                 overwrite = T, append = F,
                 driver = 'GPKG', quiet = T)
    nbcells <- length(aoi_grid)
  }
  message(paste('the crs of the output products will be', crs_target))
  plots <- get_plot_list(dsn = dsn_grid, nbdigits = nchar(nbcells))
  return(list('dsn_grid' = dsn_grid, 'plots' = plots, 'crs_target' = crs_target))
}
