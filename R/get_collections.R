#' gets collections for plot network
#'
#' @param list_aoi list. list of simple features
#' @param S2tiles list. list of S2 tiles including each sf from list_aoi
#' @param datetime list. define period 'from' / 'to' day of acquisition
#' @param output_dir character. path for output directory
#' @param overwrite boolean. should collection and S2 data be overwritten?
#' @param cloudcover numeric. maximum cloud cover over tile
#' @param collection character. collection targeted with CDSE
#' @param stac_url character. URL for STAC endpoint
#' @param nbCPU numeric. number of CPU
#' @param doublecheckColl boolean. set TRUE to double check collection
#'
#' @return list of collections per plot
#' @importFrom future.apply future_mapply
#' @importFrom future plan sequential
#' @importFrom parallel makeCluster stopCluster
#' @export
#'
get_collections <- function(list_aoi, S2tiles = NULL, datetime, output_dir,
                            overwrite = T, cloudcover = 100,
                            collection = "sentinel-2-l2a", stac_url = NULL,
                            nbCPU = 1, doublecheckColl = T){

  # create output collection directory
  collection_dir <- file.path(output_dir,'collections')
  dir.create(path = collection_dir, showWarnings = F, recursive = T)
  # define file names for collection
  FileName_fullcoll <- file.path(collection_dir,paste0('plot_',names(list_aoi),'.rds'))
  names(FileName_fullcoll) <- names(list_aoi)
  # already existing collections
  alreadyExists <- which(file.exists(FileName_fullcoll))
  # should collections be double checked?
  # if doublecheckColl = F, assumes all collections are ok of file exists
  dl_collection <- TRUE
  if (!doublecheckColl & length(alreadyExists)==length(list_aoi))
    dl_collection <- FALSE

  if (is.null(S2tiles)){
    S2tiles <- list()
    for (ni in names(list_aoi)) S2tiles[[ni]] <- NA
  }
  if (dl_collection){
    if (nbCPU==1){
      mapply(FUN = get_collection, aoi = list_aoi, S2tiles = S2tiles,
             FileName = FileName_fullcoll,
             MoreArgs = list(overwrite = overwrite,
                             datetime = datetime, collection = collection,
                             cloudcover = cloudcover, stac_url = stac_url),
             SIMPLIFY = F)
    } else if (nbCPU>1){
      cl <- parallel::makeCluster(nbCPU)
      plan("cluster", workers = cl)
      future.apply::future_mapply(FUN = get_collection,
                                  aoi = list_aoi, S2tiles = S2tiles,
                                  FileName = FileName_fullcoll,
                                  MoreArgs = list(overwrite = overwrite,
                                                  datetime = datetime,
                                                  collection = collection,
                                                  cloudcover = cloudcover,
                                                  stac_url = stac_url),
                                  future.seed = T, SIMPLIFY = F)
      parallel::stopCluster(cl)
      plan(sequential)
    }
  }
  return(FileName_fullcoll)
}
