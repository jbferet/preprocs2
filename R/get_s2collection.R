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
#' @param siteName character. name of the study site
#' @param rast_out boolean. should S2 SpatRaster be obtained as output?
#' @param additional_process additional process to be applied to S2_items once downloaded
#' @param crs_target numeric.
#' @param original_clouds should original cloud mask be used or not?
#' @param argsin list
#' @param writeoutput boolean. should output file be saved?
#' @param bands2correct character. name of bands to correct from geometry
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
                             RadiometricFilter = NULL, siteName = NULL,
                             rast_out = T, additional_process = NULL,
                             crs_target = NULL, original_clouds = TRUE,
                             argsin = NULL, writeoutput = T,
                             bands2correct = c('B8A', 'B11', 'B12')){

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
    S2_items <- mapply(FUN = download_s2collection,
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
                                       overwrite = overwrite,
                                       siteName = siteName, crs_target = crs_target,
                                       original_clouds = original_clouds,
                                       argsin = argsin, writeoutput = writeoutput,
                                       bands2correct = bands2correct),
                       SIMPLIFY = F)
  } else if (nbCPU>1){
    cl <- parallel::makeCluster(nbCPU)
    plan("cluster", workers = cl)
    handlers(global = TRUE)
    handlers("cli")
    with_progress({
      p <- progressr::progressor(steps = nbPlots)
      S2_items <- future.apply::future_mapply(FUN = download_s2collection,
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
                                                              overwrite = overwrite,
                                                              siteName = siteName,
                                                              p = p, rast_out = rast_out,
                                                              additional_process = additional_process,
                                                              crs_target = crs_target,
                                                              original_clouds = original_clouds,
                                                              argsin = argsin,
                                                              writeoutput = writeoutput,
                                                              bands2correct = bands2correct),
                                              future.seed = T,
                                              future.chunk.size = NULL,
                                              future.scheduling = structure(TRUE, ordering = "random"),
                                              SIMPLIFY = F)
    })
    parallel::stopCluster(cl)
    plan(sequential)
  }
  if (length(plots)>1) S2_items <- NULL
  return(S2_items)
}
