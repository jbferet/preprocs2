#' return S2 tiles intersecting with the footprint of a vector file
#'
#' @param plots list.
#' @param aoi_path character. path for vector file defining aoi
#' @param datetime character or date or list defining date of acquisition or time range
#' @param output_dir character. path for output directory
#' @param cloudcover numeric.
#' @param site character. name of the study site
#' @param path_S2tilinggrid character. path for the Sentinel-2_tiling_grid.kml file
#' @param overwrite boolean.
#' @param collection character.
#' @param geomAcq boolean.
#' @param nbCPU numeric.
#' @param authentication list.
#' @param mask_path character.
#' @param resolution numeric.
#' @param fraction_vegetation numeric.
#' @param stac_url character.
#' @param doublecheckColl boolean.
#' @param offset numeric.
#' @param offset_B2 boolean.
#' @param corr_BRF boolean.
#' @param RadiometricFilter list.
#' @param clean_bands boolean
#'
#' @return S2tiles list of tiles corresponding to plots
#' @importFrom sf read_sf st_intersects st_collection_extract st_write
#' @export

get_s2_tiling <- function(plots, aoi_path, datetime, output_dir, cloudcover = 100,
                          site = NULL, path_S2tilinggrid = NULL, overwrite = T,
                          collection = "sentinel-2-l2a", geomAcq = F, nbCPU = 1,
                          authentication = NULL, mask_path = NULL, resolution = 10,
                          fraction_vegetation = 5, stac_url = NULL, doublecheckColl = T,
                          offset = 1000, offset_B2 = F, corr_BRF = F,
                          RadiometricFilter = NULL, clean_bands = T){
  # create proper datetime if only one date provided
  individualAcq <- F
  if (inherits(x = datetime, what = 'Date') |
      inherits(x = datetime, what = 'character')){
    datetime <- list('from' = as.Date(datetime), 'to' = as.Date(datetime)+1)
    individualAcq <- T
  }

  # define s2 tiles corresponding to aoi
  message('get S2 tiles corresponding to aoi')
  path_S2tilinggrid <- check_S2tilinggrid(path_S2tilinggrid = path_S2tilinggrid)
  S2_grid <- get_s2_tiles(plots = plots,
                          dsn_bbox = aoi_path,
                          site = site,
                          path_S2tilinggrid = path_S2tilinggrid,
                          overwrite = overwrite)

  if (geomAcq & is.null(authentication)){
    message('Please provide authentication for CDSE if you want to get geometry of acquisition')
    message('Activate OAuth clients following this link')
    message('https://shapps.dataspace.copernicus.eu/dashboard/#/account/settings')
  } else if (geomAcq & !is.null(authentication)){
    message('get S2 geometry of acquisition of tiles overlapping with aoi')
    get_GeomAcq_s2(dsn_S2tiles = S2_grid$dsn_S2tiles,
                   datetime = datetime,
                   cloudcover = cloudcover,
                   authentication = authentication,
                   collection = collection,
                   output_dir = output_dir,
                   overwrite = overwrite,
                   nbCPU = nbCPU)
  }
  # download S2 data
  message('download S2 collection')
  get_s2collection(plots = plots,
                   datetime = datetime,
                   nbCPU = nbCPU,
                   S2tiles = S2_grid$S2tiles,
                   output_dir = output_dir,
                   cloudcover = cloudcover,
                   mask_path = mask_path,
                   fraction_vegetation = fraction_vegetation,
                   resolution = resolution,
                   collection = collection,
                   stac_url = stac_url,
                   overwrite = overwrite,
                   doublecheckColl = doublecheckColl,
                   offset = offset,
                   offset_B2 = offset_B2,
                   corr_BRF = corr_BRF,
                   RadiometricFilter = RadiometricFilter,
                   clean_bands = clean_bands)
  return(invisible())
}
