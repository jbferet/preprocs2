#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param plots list. list of polygons
#' @param dsn_bbox character. path for vector file corresponding to bbox
#' @param site character. name of the study site
#' @param overwrite boolean.
#' @param path_S2tilinggrid character. path for the Sentinel-2_tiling_grid.kml file
#'
#' @return S2tiles list of tiles corresponding to plots
#' @importFrom sf read_sf st_intersects st_collection_extract st_write
#' @export

get_s2_tiles <- function(plots, dsn_bbox, site = NULL, overwrite = T,
                         path_S2tilinggrid = 'Sentinel-2_tiling_grid.kml'){

  if (!is.null(site)) site <- paste0('_', site)
  S2tiles_path <- file.path(dirname(dsn_bbox),paste0('S2tiles',site,'.rds'))
  dsn_S2tiles_footprint <- file.path(dirname(dsn_bbox), paste0('s2_footprint',site,'.gpkg'))            # path where vector is saved
  if (file.exists(S2tiles_path) & overwrite == F) S2tiles <- readRDS(file = S2tiles_path)
  if (!file.exists(S2tiles_path) | overwrite == T){
    # get footprint for bounding box including the plot network
    footprint <- get_s2_footprint(dsn = dsn_bbox)
    # identify S2 tiles fully including plots
    S2tilingGrid <- sf::read_sf(path_S2tilinggrid)
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
    sf::st_write(obj = tilefp, dsn = dsn_S2tiles_footprint, driver = 'GPKG', delete_dsn = T)
  }
  return(list('S2tiles' = S2tiles,
              'dsn_S2tiles' = dsn_S2tiles_footprint))
}
