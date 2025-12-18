#' gets collections for plot network
#'
#' @param list_aoi list. list of simple features
#' @param s2_tiles list. list of S2 tiles including each sf from list_aoi
#' @param datetime list. define period 'from' / 'to' day of acquisition
#' @param output_dir character. path for output directory
#' @param overwrite boolean. should collection and S2 data be overwritten?
#' @param cloudcover numeric. maximum cloud cover over tile
#' @param stac_info list. stac provider, name of collection url for stac
#' @param nbCPU numeric. number of CPU
#' @param overwrite_collection boolean. set TRUE to double check collection
#'
#' @return list of collections per plot
#' @importFrom future.apply future_mapply
#' @importFrom future plan sequential
#' @importFrom parallel makeCluster stopCluster
#' @export
#'
get_collections <- function(list_aoi, s2_tiles = NULL, datetime, output_dir,
                            overwrite = TRUE, cloudcover = 100, stac_info,
                            nbCPU = 1, overwrite_collection = TRUE){

  # create output collection directory
  collection_dir <- file.path(output_dir,'collections')
  dir.create(path = collection_dir, showWarnings = FALSE, recursive = TRUE)
  # define file names for collection
  FileName_fullcoll <- file.path(collection_dir,paste0('plot_',names(list_aoi),'.rds'))
  names(FileName_fullcoll) <- names(list_aoi)
  # already existing collections
  alreadyExists <- which(file.exists(FileName_fullcoll))
  # should collections be double checked?
  # if overwrite_collection = F, assumes all collections are ok of file exists
  dl_collection <- TRUE
  if (!overwrite_collection & length(alreadyExists)==length(list_aoi))
    dl_collection <- FALSE

  if (is.null(s2_tiles)){
    s2_tiles <- list()
    for (ni in names(list_aoi))
      s2_tiles[[ni]] <- NA
  }
  if (dl_collection){
    if (nbCPU==1){
      FileName_fullcoll <- mapply(FUN = get_collection, aoi = list_aoi,
                                  s2_tiles = s2_tiles,
                                  FileName = FileName_fullcoll,
                                  MoreArgs = list(overwrite = overwrite,
                                                  datetime = datetime,
                                                  stac_info = stac_info,
                                                  cloudcover = cloudcover),
                                  SIMPLIFY = FALSE)

    } else if (nbCPU>1){
      cl <- parallel::makeCluster(nbCPU)
      plan("cluster", workers = cl)
      FileName_fullcoll <- future.apply::future_mapply(FUN = get_collection,
                                                       aoi = list_aoi, s2_tiles = s2_tiles,
                                                       FileName = FileName_fullcoll,
                                                       MoreArgs = list(overwrite = overwrite,
                                                                       datetime = datetime,
                                                                       stac_info = stac_info,
                                                                       cloudcover = cloudcover),
                                                       future.seed = TRUE, SIMPLIFY = FALSE)
      parallel::stopCluster(cl)
      plan(sequential)
    }
  }
  return(FileName_fullcoll)
}
