#' downloads a time series defined by item collection
#'
#' @param collection_path character. directory where plot collection is stored
#' @param aoi geometry corresponding to one point
#' @param iChar plot ID
#' @param raster_dir directory where rasters are stored
#' @param mask_path path for binary mask
#' @param fraction_vegetation numeric. minimum fraction vegetation over plot
#' @param collection character. collection targeted with CDSE
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param offset numeric. offset value
#' @param offset_B2 boolean.
#' @param corr_BRF boolean.
#' @param p list.
#' @param RadiometricFilter list. values for radiometric filter shade cloud vegetation
#' @param overwrite boolean.
#' @param clean_bands boolean.
#'
#' @return list of collections per plot
#' @export
#'
download_s2collection <- function(collection_path, aoi, iChar, raster_dir,
                                  mask_path = NULL, fraction_vegetation = 10,
                                  collection = 'sentinel-2-l2a', resolution = 10,
                                  offset = 1000, offset_B2 = F, corr_BRF = F,
                                  p = NULL, RadiometricFilter = NULL,
                                  overwrite = T, clean_bands = T){
  # get collection cloud masks
  get_cloudmask(collection_path = collection_path, aoi = aoi,
                iChar = iChar, raster_dir = raster_dir, overwrite = overwrite,
                fraction_vegetation = fraction_vegetation,
                collection = collection, resolution = resolution)

  # update collection cloud masks
  update_mask(aoi = aoi, collection_path = collection_path, iChar = iChar,
              raster_dir = raster_dir, mask_path = mask_path,
              fraction_vegetation = fraction_vegetation,
              offset = offset, collection = collection, resolution = resolution,
              RadiometricFilter = RadiometricFilter, overwrite = overwrite)

  # download S2 collection
  download_s2(aoi = aoi, raster_dir = raster_dir,
              collection_path = collection_path, iChar = iChar,
              resolution = resolution, offset = offset, offset_B2 = offset_B2,
              corr_BRF = corr_BRF, clean_bands = clean_bands)
  if (!is.null(p)) p()
}
