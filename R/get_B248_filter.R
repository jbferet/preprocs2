#' downloads S2 reflectance to update the vegetation mask
#'
#' @param raster_dir directory where rasters are stored
#' @param mask_path directory where rasters are stored
#' @param item_collection path for collection to download
#' @param cloudmasks spatial raster
#' @param iChar plot ID
#' @param aoi geometry corresponding to one point
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param collection character.
#' @param fraction_vegetation numeric. minimum % of vegetatiomn to have on plot footprint
#' @param offset numeric. value of S2 offset
#' @param RadiometricFilter list. list of radiometric filters for shade, clouds and vegetation
#' @param siteName character. name of the study site
#' @param crs_target numeric.
#' @param original_clouds boolean
#' @param overwrite boolean
#' @param S2_items list
#' @param writeoutput boolean. should output file be saved?
#'
#' @return list of collections per plot
#' @importFrom terra rast
#' @importFrom sf st_transform st_bbox
#' @importFrom rstac assets_url
#' @export
#'
get_B248_filter <- function(raster_dir, mask_path, item_collection, cloudmasks,
                            iChar, aoi, resolution, collection = 'sentinel-2-l2a',
                            fraction_vegetation, offset = 1000,
                            RadiometricFilter = NULL, siteName, crs_target = NULL,
                            original_clouds = original_clouds, overwrite = F,
                            S2_items = NULL, writeoutput = T){

  asset_cloud <- get_cloud_asset(item_collection, collection)
  suffix <- paste0('_',asset_cloud,'.tiff')
  acq2keep <- NULL
  asset_names <- c('B02', 'B04', 'B08')
  if (is.null(S2_items)){
    S2_items <- lapply(X = item_collection$features,
                       FUN = get_asset_terra,
                       asset_names = asset_names,
                       collection = collection,
                       aoi = aoi,
                       crs_target = crs_target)
    names(S2_items) <- names(cloudmasks)
  }

  if (collection == 'sentinel-2-l2a'){
    baseline <- lapply(lapply(item_collection$features,'[[','properties'),
                       '[[', 's2:processing_baseline')
  } else if (collection == 'sentinel2-l2a-sen2lasrc'){
    baseline <- lapply(lapply(lapply(item_collection$features,'[[','properties'),
                              '[[', 'processing:software'),
                       '[[', 'sen2lasrc')
  }
  for (i in seq_len(length(baseline))){
    if (as.numeric(baseline[[i]])>=4 & asset_cloud == 'SCL')
      S2_items[[i]] <- lapply(X = S2_items[[i]], FUN = function(x, offset){x - offset}, offset)
  }

  if (length(S2_items)>0){
    updatedMasks <- mapply(FUN = apply_radiometric_filter,
                           S2_item = S2_items,
                           cloudmask = cloudmasks,
                           acq = as.list(item_collection$acquisitionDate),
                           MoreArgs = list(raster_dir = raster_dir,
                                           mask_path = mask_path,
                                           iChar = iChar,
                                           aoiplot = aoi,
                                           fraction_vegetation = fraction_vegetation,
                                           RadiometricFilter = RadiometricFilter,
                                           asset_cloud = asset_cloud,
                                           siteName = siteName, 
                                           original_clouds = original_clouds,
                                           overwrite = overwrite, 
                                           writeoutput = writeoutput),
                           SIMPLIFY = F)

    validity <- lapply(updatedMasks, '[[',1)
    mask_update <- lapply(updatedMasks, '[[',2)
    keepit <- which(unlist(validity))
    if (length(keepit)>0) acq2keep <- item_collection$acquisitionDate[keepit]
  }

  if (!is.null(acq2keep)) {
    selacq <- match(acq2keep, item_collection$acquisitionDate)
    item_collection$acquisitionDate <- item_collection$acquisitionDate[selacq]
    item_collection$features <- item_collection$features[selacq]
    cloudmasks <- cloudmasks[selacq]
    mask_update <- mask_update[selacq]
    S2_items <- S2_items[selacq]
  } else {
    item_collection$acquisitionDate <- NULL
    item_collection$features <- NULL
    cloudmasks <- NULL
    S2_items <- NULL
  }
  return(list('cloudmask'= cloudmasks, 'mask_update'= mask_update, 
              'S2_items' = S2_items, 'collection_info' = item_collection))
}
