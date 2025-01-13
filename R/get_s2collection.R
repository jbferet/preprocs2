#' downloads Sentinel-2 time series for a vectorized plot network
#'
#' @param plots list. sf objects corresponding to polygons
#' @param S2tiles list. S2 tiles corresponding to the plots
#' @param datetime list. define period 'from' / 'to' day of acquisition
#' @param output_dir character. output directory
#' @param mask_path character. path for mask
#' @param cloudcover numeric. maximum cloud cover over tile
#' @param fraction_vegetation numeric. minimum fraction vegetation to consider acquisition
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param collection character. collection
#' @param stac_url character. STAC endpoint
#' @param overwrite boolean. should collection and S2 data be overwritten?
#' @param nbCPU numeric. number of threads to work with
#' @param doublecheckColl boolean.
#' @param offset numeric. S2 reflectance offset
#' @param offset_B2 boolean. should an offset be applied to normalize B2?
#' @param corr_BRF boolean.
#' @param RadiometricFilter list.
#' @param clean_bands boolean.
#'
#' @return list of collections per plot
#' @importFrom parallel makeCluster stopCluster
#' @importFrom future plan sequential
#' @importFrom future.apply future_mapply
#' @importFrom progressr with_progress progressor handlers
#' @export
#'
get_s2collection <- function(plots, S2tiles = NULL, datetime, output_dir,
                             mask_path = NULL, cloudcover = 100,
                             fraction_vegetation = 5, resolution = 10,
                             collection = "sentinel-2-l2a", stac_url = NULL,
                             overwrite = F, nbCPU = 1, doublecheckColl = T,
                             offset = 1000, offset_B2 = F, corr_BRF = F,
                             RadiometricFilter = NULL, clean_bands = T){
  
  # get collection for each plot
  if (length(plots)<nbCPU) nbCPU <- length(plots)
  collection_path <- get_collections(list_aoi = plots, 
                                     S2tiles = S2tiles,
                                     datetime = datetime, 
                                     output_dir = output_dir,
                                     cloudcover = cloudcover,
                                     overwrite = overwrite,
                                     collection = collection,
                                     nbCPU = nbCPU, stac_url = stac_url,
                                     doublecheckColl = doublecheckColl)
  # define paths
  raster_dir <- file.path(output_dir, 'raster_samples')
  dir.create(raster_dir, showWarnings = F, recursive = T)
  
  # identify plots and discard first ones if needed
  nbPlots <- length(plots)
  ID_aoi <- names(plots)
  if (nbCPU==1){
    s2_file <- mapply(FUN = download_s2collection,
                      collection_path = collection_path,
                      aoi = plots,
                      iChar = ID_aoi,
                      MoreArgs = list(raster_dir = raster_dir,
                                      mask_path = mask_path,
                                      fraction_vegetation = fraction_vegetation,
                                      collection = collection,
                                      resolution = resolution,
                                      offset = offset,
                                      offset_B2 = offset_B2,
                                      corr_BRF = corr_BRF,
                                      RadiometricFilter = RadiometricFilter,
                                      clean_bands = clean_bands,
                                      overwrite = overwrite), SIMPLIFY = F)
  } else if (nbCPU>1){
    cl <- parallel::makeCluster(nbCPU)
    plan("cluster", workers = cl)
    handlers(global = TRUE)
    handlers("cli")
    with_progress({
      p <- progressr::progressor(steps = nbPlots)
      s2_file <- future.apply::future_mapply(FUN = download_s2collection,
                                             collection_path = collection_path,
                                             aoi = plots,
                                             iChar = ID_aoi,
                                             MoreArgs = list(raster_dir = raster_dir,
                                                             mask_path = mask_path,
                                                             fraction_vegetation = fraction_vegetation,
                                                             collection = collection,
                                                             resolution = resolution,
                                                             offset = offset,
                                                             offset_B2 = offset_B2,
                                                             corr_BRF = corr_BRF,
                                                             RadiometricFilter = RadiometricFilter,
                                                             clean_bands = clean_bands,
                                                             overwrite = overwrite,
                                                             p = p),
                                             future.seed = T, SIMPLIFY = F)
    })
    parallel::stopCluster(cl)
    plan(sequential)
  }
  return()
}
