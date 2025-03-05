#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param aoi_path character. path for vector file defining aoi
#' @param bbox bbox
#' @param datetime character or date or list defining date of acquisition or time range
#' @param output_dir character. path for output directory
#' @param cloudcover numeric. cloud cover
#' @param siteName character. name of the study site
#' @param path_S2tilinggrid character. path for the Sentinel-2_tiling_grid.kml file
#' @param overwrite boolean.
#' @param geomAcq boolean.
#' @param authentication list. authentication to CDSE
#' @param collection character.
#' @param stac_url character.
#' @param additional_process additional process to be applied to S2_items once downloaded
#'
#' @return list of path corresponding to output files
#' @importFrom sf st_write st_bbox st_read st_crs st_transform
#' @export

get_s2_raster <- function(aoi_path = NULL, bbox = NULL, datetime, output_dir,
                          cloudcover = 100, siteName = NULL, path_S2tilinggrid = NULL, overwrite = T,
                          geomAcq = F, authentication = NULL,
                          collection = "sentinel-2-l2a", stac_url = NULL,
                          additional_process = NULL){

  input_dir <- NULL
  # get bounding box for aoi
  if (is.null(bbox) | ! inherits(x = bbox, 'bbox')){
    if (is.null(aoi_path)){
      message('provide path for valid vector file as "aoi_path", or "bbox" sf object as input for "get_s2_raster"')
      stop_quietly()
    } else if (file.exists(aoi_path)){
      bbox <- sf::st_bbox(sf::st_read(aoi_path, quiet= T))
      input_dir <- dirname(aoi_path)
    }
  } else if (inherits(x = bbox, 'bbox')){
    if (is.null(aoi_path)){
      input_dir <- output_dir
    } else if (file.exists(aoi_path)){
      message('both "aoi_path" and "bbox" defined as input for "get_s2_raster"')
      message('"aoi_path" will be used')
      bbox <- sf::st_bbox(sf::st_read(aoi_path, quiet= T))
      input_dir <- dirname(aoi_path)
    } else {
      input_dir <- output_dir
    }
  }
  if (is.null(input_dir)){
    message('Please provide valid "aoi_path" and "bbox" as input for "get_s2_raster"')
    stop_quietly()
  }
  dir.create(path = input_dir, showWarnings = F, recursive = T)
  if (!is.null(aoi_path)){
    crs_final <- sf::st_crs(sf::st_read(aoi_path, quiet= T))
  } else if (!is.null(bbox)){
    crs_final <- sf::st_crs(bbox)
  }
  bbox <- bbox |>
    sf::st_transform(crs_final)
  plots <- list('001' = bbox_to_poly(x = bbox, crs = crs_final))
  plots <- lapply(X = plots, FUN = sf::st_zm)
  bbox_path <- file.path(input_dir,'aoi_bbox.GPKG')
  if (!file.exists(bbox_path) | overwrite == T)
    sf::st_write(obj = plots$`001`, dsn = bbox_path,
                 driver = 'GPKG', delete_dsn = T, quiet = T)

  # create proper datetime if only one date provided
  if (inherits(x = datetime, what = 'Date') | inherits(x = datetime, what = 'character'))
    datetime <- list('from' = as.Date(datetime), 'to' = as.Date(datetime)+1)

  # define s2 tiles corresponding to aoi
  message('get S2 tiles corresponding to aoi')
  path_S2tilinggrid <- check_S2tilinggrid(path_S2tilinggrid = path_S2tilinggrid)
  S2_grid <- get_s2_tiles(plots = plots,
                          dsn_bbox = bbox_path,
                          siteName = siteName,
                          path_S2tilinggrid = path_S2tilinggrid,
                          overwrite = overwrite)

  path_geomfiles <- 'No geometry files requested'
  if (geomAcq & is.null(authentication)){
    message('Please provide authentication for CDSE if you want to get geometry of acquisition')
    message('Activate OAuth clients following this link')
    message('https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings')
  } else if (geomAcq & !is.null(authentication)){
    message('get S2 geometry of acquisition of tiles overlapping with aoi')
    path_geomfiles <- get_GeomAcq_s2(dsn_S2tiles = S2_grid$dsn_S2tiles,
                                     datetime = datetime,
                                     cloudcover = cloudcover,
                                     authentication = authentication,
                                     output_dir = output_dir,
                                     overwrite = overwrite)
  }

  # download S2 data
  message('download S2 collection')
  S2tiles <- S2_grid$S2tiles
  S2_items <- get_s2collection(plots = plots,
                               datetime = datetime,
                               S2tiles = S2tiles,
                               output_dir = output_dir,
                               collection = collection,
                               stac_url = stac_url,
                               siteName = siteName,
                               additional_process = additional_process)

  raster_dir <- file.path(output_dir, 'raster_samples')
  dir.create(path = raster_dir, showWarnings = F, recursive = T)
  if (is.null(siteName)) prefix <- file.path(raster_dir, paste0('plot_001_',names(S2_items$`001`)))
  if (!is.null(siteName)) prefix <- file.path(raster_dir, paste0(siteName,'_001_',names(S2_items$`001`)))

  if (collection =='sentinel-2-l2a') provider_mask <- 'SCL'
  if (collection =='sentinel2-l2a-sen2lasrc') provider_mask <- 'CLM'
  files_out <- list('Refl_L2A' = paste0(prefix, '.tiff'),
                    'Binary_mask' = paste0(prefix, '_BIN.tiff'),
                    'vegetation_mask' = paste0(prefix, '_BIN_v2.tiff'),
                    'provider_mask' = paste0(prefix, '_',provider_mask,'.tiff'),
                    'geometryAcquisition' = path_geomfiles)
  return(files_out)
}
