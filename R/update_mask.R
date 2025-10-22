#' update mask based on radiometric tresholds
#'
#' @param aoi geometry corresponding to one point
#' @param collection_path path for colection previously saved
#' @param iChar plot ID
#' @param raster_dir directory where rasters are stored
#' @param cloudmasks spatial raster
#' @param mask_path directory where mask is
#' @param fraction_vegetation numeric. minimum fraction vegetation over plot
#' @param collection character. collection targeted with CDSE
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param offset numeric. offset
#' @param overwrite boolean.
#' @param RadiometricFilter list.
#' @param siteName character. name of the study site
#' @param crs_target numeric.
#' @param original_clouds boolean
#' @param S2_items list
#' @param writeoutput boolean. should output file be saved?
#'
#' @return list of collections per plot
#' @importFrom terra rast values writeRaster
#' @export
#'
update_mask <- function(aoi, collection_path, iChar, raster_dir, cloudmasks,
                        mask_path = NULL, fraction_vegetation = 5,
                        collection = "sentinel-2-l2a", resolution = 10,
                        offset = 1000, overwrite = FALSE,
                        RadiometricFilter = NULL, siteName = NULL,
                        crs_target = NULL, original_clouds= TRUE,
                        S2_items = NULL, writeoutput = TRUE){


  item_collection <- readRDS(file = collection_path)
  # get asset name for cloud data
  asset_names <- get_cloud_asset(item_collection, collection)
  cloudmask <- mask_update <- NULL
  if (!is.null(cloudmasks)){
    # download B02, B04 & B08 to update mask
    maskUD_out <- get_B248_filter(raster_dir = raster_dir, mask_path = mask_path,
                                  item_collection = item_collection,
                                  cloudmasks = cloudmasks,
                                  iChar = iChar, aoi = aoi,
                                  resolution = resolution,
                                  collection = collection,
                                  fraction_vegetation = fraction_vegetation,
                                  offset = offset, siteName = siteName,
                                  RadiometricFilter = RadiometricFilter,
                                  crs_target = crs_target,
                                  original_clouds = original_clouds,
                                  overwrite = overwrite, S2_items = S2_items,
                                  writeoutput = writeoutput)
    item_collection <- maskUD_out$collection_info

    # combine list of SCL already downloaded and SCL to download
    mask_update <- maskUD_out$mask_update
    cloudmask <- maskUD_out$cloudmask
    S2_items <- maskUD_out$S2_items
    # check if sufficient vegetation fraction cover
    elim <- NULL
    for (dateAcq in names(cloudmask)){
      # if using cloudmask allowing identifying vegetation pixels
      if (asset_names == 'SCL'){
        total_pix <- length(which((terra::values(cloudmask[[dateAcq]]))>0))
        vegetation_pix <- length(which((terra::values(cloudmask[[dateAcq]]))==4))
        if (total_pix>0)
          fcover <- 100*vegetation_pix/total_pix
        if (total_pix==0)
          fcover <- 0
        # if not, eliminate day and cloudmask
        if (fcover < fraction_vegetation){
          elim <- which(item_collection$acquisitionDate %in% dateAcq)
          item_collection$features <- item_collection$features[-elim]
          item_collection$acquisitionDate <- item_collection$acquisitionDate[-elim]
          mask_update <- mask_update[-elim]
          cloudmask <- cloudmask[-elim]
          S2_items <- S2_items[-elim]
        }
      }
    }
    # if (!is.null(elim)) saveRDS(object = item_collection, file = collection_path)
    saveRDS(object = item_collection, file = collection_path)
  }
  gc()
  return(list('cloudmask'= cloudmask, 'mask_update'= mask_update,
              'S2_items' = S2_items, 'collection_info' = item_collection))
}
