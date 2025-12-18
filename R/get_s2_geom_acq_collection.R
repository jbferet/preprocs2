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

get_s2_geom_acq_collection <- function(aoi, datetime,
                                       collection = "sentinel-2-l2a",
                                       OAuthToken, cloudcover,
                                       output_dir, overwrite = FALSE){
  # create directories
  collection_dir_geom <- file.path(output_dir, 'collections', 'geom_acq_S2')
  dir.create(collection_dir_geom, showWarnings = FALSE, recursive = TRUE)
  FileName <- file.path(collection_dir_geom,'collection_CDSE_geometry.rds')
  if (! file.exists(FileName) | overwrite==TRUE){
    collection_geom_acq_S2 <- CDSE::SearchCatalog(aoi = sf::st_zm(aoi),
                                                  from = datetime$from, to = datetime$to,
                                                  collection = collection,
                                                  filter = paste0("eo:cloud_cover < ",cloudcover),
                                                  token = OAuthToken)
    # filter tile
    sel <- which(get_tile(collection_geom_acq_S2$sourceId) %in% aoi$Name)
    collection_geom_acq_S2 <- collection_geom_acq_S2[sel,]
    saveRDS(object = collection_geom_acq_S2, file = FileName)
  } else {
    collection_geom_acq_S2 <- readRDS(file = FileName)
  }
  return(collection_geom_acq_S2)
}
