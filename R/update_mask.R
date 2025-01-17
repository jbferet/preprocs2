#' update mask based on radiometric tresholds
#'
#' @param aoi geometry corresponding to one point
#' @param collection_path path for colection previously saved
#' @param iChar plot ID
#' @param raster_dir directory where rasters are stored
#' @param mask_path directory where mask is
#' @param fraction_vegetation numeric. minimum fraction vegetation over plot
#' @param collection character. collection targeted with CDSE
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param offset numeric. offset
#' @param overwrite boolean.
#' @param RadiometricFilter list.
#'
#' @return list of collections per plot
#' @importFrom terra rast values writeRaster
#' @export
#'
update_mask <- function(aoi, collection_path, iChar, raster_dir,
                        mask_path = NULL, fraction_vegetation = 10,
                        collection = "sentinel-2-l2a", resolution = 10,
                        offset = 1000, overwrite = F, RadiometricFilter = NULL){

  # days of acquisition in ascending order
  item_collection <- readRDS(file = collection_path)
  if (collection=='sentinel2-l2a-sen2lasrc'){
    item_collection <- item_collection |>
      rstactheia::items_sign_theia()
  } else if (collection=='sentinel-2-l2a'){
    item_collection <- item_collection |>
      rstac::items_sign(
        rstac::sign_planetary_computer()
      )
  }

  # get asset name for cloud data
  asset_names <- get_cloud_asset(item_collection, collection)
  suffix <- paste0('_',asset_names,'.tiff')

  # check cloud masks: are there already downloaded ones, are there missing ones?
  cloud_status <- list_cloud_dl(raster_dir, item_collection$acquisitionDate,
                                iChar, asset_names = asset_names)

  # download B02, B04 & B08 to update mask
  maskUD_out <- get_B248_filter(raster_dir = raster_dir, mask_path = mask_path,
                                collection_path = collection_path,
                                iChar = iChar, aoi = aoi, resolution = resolution,
                                collection = collection,
                                fraction_vegetation = fraction_vegetation,
                                offset = offset,
                                RadiometricFilter = RadiometricFilter)
  item_collection <- maskUD_out$collection_info

  # combine list of SCL already downloaded and SCL to download
  cloudmask <- list()
  for (d in item_collection$acquisitionDate){
    dd <- as.character(as.Date(d))
    if (dd %in% cloud_status$days_dl) cloudmask[[as.character(as.Date(d))]] <- maskUD_out$cloudmask[[dd]]
    if (dd %in% cloud_status$days_ok){
      cloudmask_path <- file.path(raster_dir,paste0('plot_',iChar,'_',as.Date(d), suffix))
      cloudmask[[dd]] <- terra::rast(cloudmask_path)
    }
  }
  # check if sufficient vegetation fraction cover
  elim <- NULL
  for (d in as.Date(item_collection$acquisitionDate)){
    dd <- as.character(as.Date(d))
    filename <- file.path(raster_dir,paste0('plot_',iChar,'_',dd, suffix))
    # if using cloudmask allowing identifying vegetation pixels
    if (asset_names == 'SCL'){
      total_pix <- length(which((terra::values(cloudmask[[dd]]))>0))
      vegetation_pix <- length(which((terra::values(cloudmask[[dd]]))==4))
      if (total_pix>0) fcover <- 100*vegetation_pix/total_pix
      if (total_pix==0) fcover <- 0
      # if not, eliminate day and cloudmask
      if (fcover < fraction_vegetation){
        elim <- which(item_collection$acquisitionDate %in% dd)
        item_collection$features <- item_collection$features[-elim]
        item_collection$acquisitionDate <- item_collection$acquisitionDate[-elim]
        cloudmask <- cloudmask[-which(names(cloudmask) %in% as.Date(d))]
        if (file.exists(filename)) unlink(filename)
      } else if  (fcover > fraction_vegetation){
        # if (!file.exists(filename) | overwrite == T)
        if (!file.exists(filename))
            terra::writeRaster(x = cloudmask[[dd]], filename = filename, overwrite = overwrite)
      }
    } else if (asset_names == 'CLM'){
      # if (!file.exists(filename) | overwrite == T)
      if (!file.exists(filename))
        terra::writeRaster(x = cloudmask[[dd]], filename = filename, overwrite = overwrite)
    }
  }
  if (!is.null(elim)) saveRDS(object = item_collection, file = collection_path)
  rm(cloudmask)
  return(invisible())
}
