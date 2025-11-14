#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param aoi_path character. path for vector file defining aoi
#' @param bbox bbox
#' @param datetime character or date or list defining date of acquisition or time range
#' @param output_dir character. path for output directory
#' @param site_name character. name of the study site
#' @param stac_info list. stac provider, name of collection url for stac
#' @param options may include
#'  - cloudcover numeric. cloud cover
#'  - path_S2_tiling_grid character. path for the Sentinel-2_tiling_grid.kml file
#'  - overwrite boolean.
#'  - geom_acq boolean.
#'  - collection character.
#'  - stac_url character.
#'  - additional_process additional process to be applied to S2_items once downloaded
#'  - bands_to_correct character. name of bands to correct from geometry
#'  - fraction_vegetation numeric. minimum fraction vegetation to consider acquisition
#'  - radiometric_filter list.
#'
#' @return list of path corresponding to output files
#' @importFrom sf st_write st_bbox st_read st_crs st_transform
#' @export

get_s2_raster <- function(aoi_path = NULL, bbox = NULL, datetime, output_dir,
                          site_name = NULL, stac_info = NULL, options = NULL){

  # set default options when not defined
  options <- set_options_preprocS2(fun = 'get_s2_raster', options = options)
  # make sure output directory is created
  dir.create(path = output_dir, showWarnings = FALSE, recursive = TRUE)

  # get plots and footprint for bbox or aoi
  aoi <- get_footprint_from_bbox_aoi(aoi_path = aoi_path, bbox = bbox,
                                     output_dir = output_dir)
  plots <- aoi$plots

  # create proper datetime if only one date provided
  if (inherits(x = datetime, what = 'Date') | inherits(x = datetime, what = 'character'))
    datetime <- list('from' = as.Date(datetime), 'to' = as.Date(datetime)+1)

  # define s2 tiles corresponding to aoi
  message('get S2 tiles corresponding to aoi')
  path_S2_tiling_grid <- check_s2_tiling_grid(path_S2_tiling_grid = options$path_S2_tiling_grid)
  S2_grid <- get_s2_tiles(plots = plots, output_dir = output_dir,
                          dsn_bbox = aoi$footprint_path, site_name = site_name,
                          path_S2_tiling_grid = path_S2_tiling_grid,
                          overwrite = options$overwrite)

  path_geomfiles <- 'No geometry files requested'
  # get token for authentication on CDSE
  OAuth_client <- get_OAuth_client()
  id <- OAuth_client$id
  pwd <- OAuth_client$pwd
  # id <- Sys.getenv("PREPROCS2_CDSE_ID")
  # pwd <- Sys.getenv("PREPROCS2_CDSE_SECRET")
  if (options$geom_acq & (nchar(id)==0 | nchar(pwd)==0)){
    message('Please provide authentication for CDSE if you want to get geometry of acquisition')
    message('Activate OAuth clients following this link')
    message('https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings')
  } else if (options$geom_acq & nchar(id)>0 & nchar(pwd)>0){
    message('get S2 geometry of acquisition of tiles overlapping with aoi')
    path_geomfiles <- get_s2_geom_acq(dsn_s2_tiles = S2_grid$dsn_s2_tiles,
                                      datetime = datetime,
                                      output_dir = output_dir,
                                      cloudcover = options$cloudcover,
                                      overwrite = options$overwrite)
  }

  # download S2 data
  message('download S2 collection')
  stac_info <- get_stac_info(stac_info)
  s2_tiles <- S2_grid$s2_tiles
  S2_items <- get_s2_collection(plots = plots,
                                datetime = datetime,
                                s2_tiles = s2_tiles,
                                output_dir = output_dir,
                                stac_info = stac_info,
                                site_name = site_name,
                                additional_process = options$additional_process,
                                bands_to_correct = options$bands_to_correct,
                                radiometric_filter = options$radiometric_filter,
                                fraction_vegetation = options$fraction_vegetation,
                                overwrite = options$overwrite)

  raster_dir <- file.path(output_dir, 'raster_samples')
  dir.create(path = raster_dir, showWarnings = F, recursive = T)
  if (is.null(site_name))
    prefix <- file.path(raster_dir, paste0('plot_001_',names(S2_items$`001`)))
  if (!is.null(site_name))
    prefix <- file.path(raster_dir, paste0(site_name,'_001_',
                                           names(S2_items$`001`)))

  if (stac_info$collection =='sentinel-2-l2a')
    provider_mask <- 'SCL'
  if (stac_info$collection =='sentinel2-l2a-sen2lasrc')
    provider_mask <- 'CLM'
  if (stac_info$collection =='sentinel2-l2a-theia')
    provider_mask <- 'CLM_R1'
  files_out <- list('Refl_L2A' = paste0(prefix, '.tiff'),
                    'Binary_mask' = paste0(prefix, '_BIN.tiff'),
                    'vegetation_mask' = paste0(prefix, '_BIN_v2.tiff'),
                    'provider_mask' = paste0(prefix, '_',provider_mask,'.tiff'),
                    'geometryAcquisition' = path_geomfiles)
  return(files_out)
}
