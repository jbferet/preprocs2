#' geometry of acquisition for aoi
#'
#' @param dsn_S2tiles character. path for vector including all S2 tiles intersecting with aoi
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

get_GeomAcq_s2 <- function(dsn_S2tiles, datetime, cloudcover = 100,
                           collection = "sentinel-2-l2a", output_dir,
                           nbCPU = 1, overwrite = F){

  # create directory where geometry of acquisition rasters are stored
  geom_dir <- file.path(output_dir, 'geomAcq_S2')
  dir.create(geom_dir, showWarnings = F, recursive = T)
  # read aoi
  aoi <- sf::st_read(dsn = dsn_S2tiles, as_tibble = F, quiet = T)
  # get token for CDSE
  id <- Sys.getenv("PREPROCS2_CDSE_ID")
  pwd <- Sys.getenv("PREPROCS2_CDSE_SECRET")
  if (nchar(id)==0 | nchar(pwd)==0){
    path_geomfiles <- NULL
    message('please follow install procedure for preprocS2 and provide OAuth')
    message('https://jbferet.gitlab.io/preprocs2/')
    message('no authentication possible to CDSE')
  } else {
    OAuthToken <- CDSE::GetOAuthToken(id = id,
                                      secret = pwd)
    # get collection for geometry of acquisition corresponding to the aoi
    collection_GeomAcq_S2 <- get_GeomAcq_s2_coll(aoi = aoi, datetime = datetime,
                                                 collection = collection,
                                                 OAuthToken = OAuthToken,
                                                 cloudcover = cloudcover,
                                                 output_dir = output_dir,
                                                 overwrite = overwrite)
    # download geometry of acquisition corresponding to the aoi stored in collection
    path_geomfiles <- download_GeomAcq_s2(aoi = aoi,
                                          geom_dir = geom_dir,
                                          collection_GeomAcq_S2 = collection_GeomAcq_S2,
                                          nbCPU = nbCPU)
  }
  return(path_geomfiles)
}

