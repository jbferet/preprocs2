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
#' @param keepCRS boolean.
#'
#' @return list of path corresponding to output files
#' @importFrom sf read_sf st_intersects st_collection_extract st_write
#' @export

get_s2_raster <- function(aoi_path = NULL, bbox = NULL, datetime, output_dir, cloudcover = 100,
                          siteName = NULL, path_S2tilinggrid = NULL, overwrite = T,
                          geomAcq = F, authentication = NULL,
                          collection = "sentinel-2-l2a", stac_url = NULL,
                          keepCRS = T){

  input_dir <- NULL
  # get bounding box for aoi
  if (is.null(bbox) | ! inherits(x = bbox, 'bbox')){
    if (is.null(aoi_path)){
      message('provide path for valid vector file as "aoi_path", or "bbox" sf object as input for "get_s2_raster"')
      stop_quietly()
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
  if (is.null(input_dir)){
    message('Please provide valid "aoi_path" and "bbox" as input for "get_s2_raster"')
    stop_quietly()
  }
  dir.create(path = input_dir, showWarnings = F, recursive = T)
  if (keepCRS == F){
    crs_final <- 4326
  } else {
    if (!is.null(aoi_path)){
      crs_final <- sf::st_crs(sf::st_read(aoi_path))
    } else if (!is.null(bbox)){
      crs_final <- sf::st_crs(bbox)
    }
  }
  bbox <- bbox |>
    sf::st_transform(crs_final)
  plots <- list('001' = bbox_to_poly(x = bbox, crs = crs_final))
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
  S2_grid <- get_s2_tiles(plots = plots,
                          dsn_bbox = bbox_path,
                          site = siteName,
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
  get_s2collection(plots = plots, datetime = datetime,
                   S2tiles = S2tiles, output_dir = output_dir,
                   collection = collection, stac_url = stac_url)

  # change name if siteName provided
  raster_dir <- file.path(output_dir, 'raster_samples')
  list_files <- list.files(path = raster_dir, pattern = 'plot_001', full.names = T)

  rm(list=setdiff(ls(),c('siteName', 'list_files', 'path_geomfiles')))
  gc()
  if (!is.null(siteName)){
    name_update <- gsub(pattern = 'plot_001', replacement = siteName, x = list_files)
    suppress <- suppressWarnings(file.rename(from = list_files, to = name_update))
    if (FALSE %in% suppress){
      for (ii in seq_len(length(suppress))){
        if (suppress[ii] ==F)
          file.copy(from = list_files[ii], to = name_update[ii])
      }
    }
    list_files <- name_update
  }
  files_out <- list('Refl_L2A' = list_files[1],
                    'Binary_mask' = list_files[2],
                    'vegetation_mask' = list_files[3],
                    'provider_mask' = list_files[4],
                    'geometryAcquisition' = path_geomfiles)
  return(files_out)
}
