#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param plots list. list of polygons
#' @param dsn_bbox character. path for vector file corresponding to bbox
#' @param output_dir character. output path
#' @param siteName character. name of the study site
#' @param overwrite boolean.
#' @param path_S2tilinggrid character. path for the Sentinel-2_tiling_grid.kml file
#'
#' @return S2tiles list of tiles corresponding to plots
#' @importFrom sf read_sf st_intersects st_collection_extract st_write
#' @export

get_s2_tiles <- function(plots, dsn_bbox, output_dir, siteName = NULL, overwrite = T,
                         path_S2tilinggrid = 'Sentinel-2_tiling_grid.kml'){

  # add site name if provided
  if (!is.null(siteName))
    siteName <- paste0('_', siteName)
  # define path for output files
  S2tiles_path <- file.path(output_dir,paste0('S2tiles',siteName,'.rds'))
  dsn_S2tiles_footprint <- file.path(output_dir, paste0('s2_footprint',siteName,'.gpkg'))            # path where vector is saved
  # read S2 tile info file if exists
  if (file.exists(S2tiles_path) & overwrite == F)
    S2tiles <- readRDS(file = S2tiles_path)
  if (!file.exists(S2tiles_path) | overwrite == T){
    # get footprint for bounding box including the plot network
    footprint <- get_s2_footprint(dsn = dsn_bbox)
    # identify S2 tiles fully including plots
    S2tilingGrid <- sf::read_sf(path_S2tilinggrid, quiet = T)
    if (! sf::st_crs(S2tilingGrid) == sf::st_crs(footprint))
      footprint <- sf::st_transform(x = footprint, crs = sf::st_crs(S2tilingGrid))

    S2tilingGrid$geometry <- sf::st_zm(S2tilingGrid$geometry)
    intersection <- sf::st_intersects(x = footprint, y = S2tilingGrid$geometry)
    S2tilingGridsub <- S2tilingGrid[intersection[[1]],]
    # identify S2 tiles corresponding to each plot
    if (inherits(x = plots, what = 'list')){
      S2tiles <- lapply(X = plots, FUN = get_plot_tile, S2tilingGridsub)
    } else if (inherits(x = plots[[1]], what = c('sf', 'sfc'))){
      S2tiles <- get_plot_tile(aoi = plots, S2_footprint = S2tilingGridsub)
    } else {
      message('get_s2_tiles expects simple feature: sf, sfc or list of sf/sfc')
      stop()
    }
    # save S2 tiles corresponding to each cell grid
    saveRDS(object = S2tiles, file = S2tiles_path)
    # save S2 tiles intersecting with aoi as gpkg
    tilefp <- sf::st_collection_extract(x = S2tilingGridsub, type = "POLYGON")
    sf::st_write(obj = tilefp, dsn = dsn_S2tiles_footprint, driver = 'GPKG',
                 delete_dsn = T, quiet = T)
  }
  return(list('S2tiles' = S2tiles,
              'dsn_S2tiles' = dsn_S2tiles_footprint))
}
