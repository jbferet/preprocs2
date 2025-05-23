#' get collection for geometry of acquisition corresponding to the aoi
#'
#' @param aoi spatial feature. vector including all S2 tiles intersecting with aoi
#' @param datetime list. define period 'from' / 'to' day of acquisition
#' @param collection character. collection targeted with CDSE
#' @param OAuthToken authentication token for CDSE
#' @param cloudcover numeric. maximum cloud cover over tile
#' @param output_dir character. main output directory
#' @param overwrite boolean. should collection and S2 data be overwritten?
#'
#' @return list of collections per plot
#' @importFrom sf st_zm
#' @importFrom CDSE SearchCatalog
#' @export

get_GeomAcq_s2_coll <- function(aoi, datetime, collection = "sentinel-2-l2a",
                                OAuthToken, cloudcover, output_dir, overwrite = F){
  # create directories
  collection_dir_geom <- file.path(output_dir, 'collections', 'geomAcq_S2')
  dir.create(collection_dir_geom, showWarnings = F, recursive = T)
  FileName <- file.path(collection_dir_geom,'collection_CDSE_geometry.rds')
  if (! file.exists(FileName) | overwrite==T){
    collection_GeomAcq_S2 <- CDSE::SearchCatalog(aoi = sf::st_zm(aoi),
                                                 from = datetime$from, to = datetime$to,
                                                 collection = collection,
                                                 filter = paste0("eo:cloud_cover < ",cloudcover),
                                                 token = OAuthToken)
    # filter tile
    sel <- which(get_tile(collection_GeomAcq_S2$sourceId) %in% aoi$Name)
    collection_GeomAcq_S2 <- collection_GeomAcq_S2[sel,]
    saveRDS(object = collection_GeomAcq_S2, file = FileName)
  } else {
    collection_GeomAcq_S2 <- readRDS(file = FileName)
  }
  return(collection_GeomAcq_S2)
}
