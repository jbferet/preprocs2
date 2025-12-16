#' geometry of acquisition for aoi
#'
#' @param dsn_s2_tiles character. path for vector including all S2 tiles intersecting with aoi
#' @param datetime list. define period 'from' / 'to' day of acquisition
#' @param cloudcover numeric. maximum cloud cover over tile
#' @param collection character. collection targeted with CDSE
#' @param output_dir character. main output directory
#' @param nbCPU numeric. number of CPUs
#' @param overwrite boolean. should collection and S2 data be overwritten?
#'
#' @return list of collections per plot
#' @importFrom CDSE GetOAuthToken
#' @importFrom sf st_read
#' @export

get_s2_geom_acq <- function(dsn_s2_tiles, datetime, cloudcover = 100,
                            collection = "sentinel-2-l2a", output_dir,
                            nbCPU = 1, overwrite = F){

  # create directory where geometry of acquisition rasters are stored
  geom_dir <- file.path(output_dir, 'geom_acq_S2')
  dir.create(geom_dir, showWarnings = F, recursive = T)
  # read aoi
  aoi <- sf::st_read(dsn = dsn_s2_tiles, as_tibble = F, quiet = T)
  # get token for authentication on CDSE
  OAuth_client <- get_OAuth_client()
  id <- OAuth_client$id
  pwd <- OAuth_client$pwd
  # id <- Sys.getenv("PREPROCS2_CDSE_ID")
  # pwd <- Sys.getenv("PREPROCS2_CDSE_SECRET")
  if (nchar(id)==0 | nchar(pwd)==0){
    path_geomfiles <- NULL
    message('please follow install procedure for preprocS2 and provide OAuth')
    message('https://jbferet.gitlab.io/preprocs2/')
    message('no authentication possible to CDSE')
  } else {
    OAuthToken <- CDSE::GetOAuthToken(id = id,
                                      secret = pwd)
    # get collection for geometry of acquisition corresponding to the aoi
    collection_geom_acq_S2 <- get_s2_geom_acq_collection(aoi = aoi, datetime = datetime,
                                                         collection = collection,
                                                         OAuthToken = OAuthToken,
                                                         cloudcover = cloudcover,
                                                         output_dir = output_dir,
                                                         overwrite = overwrite)
    # download geometry of acquisition corresponding to the aoi stored in collection
    path_geomfiles <- download_s2_geom_acq(aoi = aoi,
                                           geom_dir = geom_dir,
                                           collection_geom_acq_S2 = collection_geom_acq_S2,
                                           nbCPU = nbCPU)
  }
  return(path_geomfiles)
}

