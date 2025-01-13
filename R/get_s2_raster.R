#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param aoi_path character. path for vector file defining aoi
#' @param bbox bbox
#' @param datetime character or date or list defining date of acquisition or time range
#' @param output_dir character. path for output directory
#' @param cloudcover numeric. cloud cover
#' @param site character. name of the study site
#' @param path_S2tilinggrid character. path for the Sentinel-2_tiling_grid.kml file
#' @param overwrite boolean.
#' @param geomAcq boolean.
#' @param authentication list. authentication to CDSE
#' @param collection character.
#' @param stac_url character.
#'
#' @return S2tiles list of tiles corresponding to plots
#' @importFrom sf read_sf st_intersects st_collection_extract st_write
#' @export

get_s2_raster <- function(aoi_path = NULL, bbox = NULL, datetime, output_dir, cloudcover = 100,
                          site = NULL, path_S2tilinggrid = NULL, overwrite = T,
                          geomAcq = F, authentication = NULL, 
                          collection = "sentinel-2-l2a", stac_url = NULL){

  # get bounding box for aoi
  if (is.null(bbox) | ! inherits(x = bbox, 'bbox')){
    if (is.null(aoi_path)){
      message('provide path for valid vector file as "aoi_path", or "bbox" sf object as input for "get_s2_raster"')
      stop()
    } else if (file.exists(aoi_path)){
      bbox <- sf::st_bbox(sf::st_read(aoi_path))
      input_dir <- dirname(aoi_path)
    }
  } else if (inherits(x = bbox, 'bbox')){
    if (is.null(aoi_path)){
      input_dir <- output_dir
    } else if (file.exists(aoi_path)){
      message('both "aoi_path" and "bbox" defined as input for "get_s2_raster"')
      message('"aoi_path" will be used')
      bbox <- sf::st_bbox(sf::st_read(aoi_path))
      input_dir <- dirname(aoi_path)
    } else {
      input_dir <- output_dir
    }
  }
  dir.create(path = input_dir, showWarnings = F, recursive = T)
  plots <- list('001' = bbox_to_poly(x = bbox))
  bbox_path <- file.path(input_dir,'aoi_bbox.GPKG')
  if (!file.exists(bbox_path) | overwrite == T)
    sf::st_write(obj = plots$`001`, dsn = bbox_path,
                 driver = 'GPKG', delete_dsn = T)

  # create proper datetime if only one date provided
  if (inherits(x = datetime, what = 'Date') | inherits(x = datetime, what = 'character'))
    datetime <- list('from' = as.Date(datetime), 'to' = as.Date(datetime)+1)

  # define s2 tiles corresponding to aoi
  message('get S2 tiles corresponding to aoi')
  path_S2tilinggrid <- check_S2tilinggrid(path_S2tilinggrid = path_S2tilinggrid)
  S2_grid <- get_s2_tiles(plots = plots, dsn_bbox = bbox_path, site = site,
                          path_S2tilinggrid = path_S2tilinggrid,
                          overwrite = overwrite)

  if (geomAcq & is.null(authentication)){
    message('Please provide authentication for CDSE if you want to get geometry of acquisition')
    message('Activate OAuth clients following this link')
    message('https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings')
  } else if (geomAcq & !is.null(authentication)){
    message('get S2 geometry of acquisition of tiles overlapping with aoi')
    get_GeomAcq_s2(dsn_S2tiles = S2_grid$dsn_S2tiles, datetime = datetime,
                   cloudcover = cloudcover, authentication = authentication,
                   output_dir = output_dir, overwrite = overwrite)
  }

  # download S2 data
  message('download S2 collection')
  get_s2collection(plots = plots, datetime = datetime,
                   S2tiles = S2_grid$S2tiles, output_dir = output_dir, 
                   collection = collection, stac_url = stac_url)
  # get_s2collection(plots = plots, datetime = datetime,
  #                  S2tiles = S2_grid$S2tiles, output_dir = output_dir)
  return(invisible())
}
