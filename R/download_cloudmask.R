#' downloads SCL from Planetary
#'
#' @param aoi geometry corresponding to one point
#' @param raster_dir directory where rasters are stored
#' @param item_collection description of collection to download
#' @param iChar plot ID
#' @param collection character. name of the collection
#' @param asset_names character.
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param siteName character. name of the study site
#'
#' @return list of collections per plot
#' @importFrom rstac assets_url
#' @importFrom terra rast resample
#' @importFrom sf st_transform st_bbox
#' @export
#'
download_cloudmask <- function(aoi, raster_dir, item_collection, iChar,
                               collection = 'sentinel-2-l2a', asset_names,
                               resolution, siteName = NULL){

  # if collection not empty
  if (length(item_collection$acquisitionDate)>0){
    # stac_source <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    # define file name for cloud mask
    S2product <- unlist(lapply(item_collection$features,'[[','id'))
    dateAcq <- get_dateAcq(S2product = S2product, collection = collection)
    if (is.null(siteName)){
      filenames <- basename(paste0('plot_',iChar,'_',dateAcq,'_',asset_names,'.tiff'))
    } else {
      filenames <- basename(paste0(siteName,'_',iChar,'_',dateAcq,'_',asset_names,'.tiff'))
    }
    filepath <- file.path(raster_dir,filenames)
    # check if expected files already exist
    exist_file <- file.exists(filepath)
    # if cloud file does not exists
    features_dl <- features_exist <- NULL
    if (FALSE %in% exist_file){
      # keep downloading these files
      keep_dl <- which(exist_file==FALSE)
      # corresponding acquisition date
      keep_dateAcq <- dateAcq[keep_dl]
      item_collection <- item_collection$features[keep_dl]
      # download
      features_dl <- lapply(X = item_collection, 
                            FUN = get_asset_terra, 
                            asset_names = asset_names, 
                            collection = collection, 
                            aoi = aoi)
      names(features_dl) <- keep_dateAcq
      for (i in seq_len(length(features_dl)))
        features_dl[[i]] <- features_dl[[i]][[1]]
    }
    if (TRUE %in% exist_file){
      read_exists <- which(exist_file==TRUE)
      features_exist <- lapply(filepath, terra::rast)
      names(features_exist) <- dateAcq[read_exists]
      item_collection <- item_collection$features
    }
    if (is.null(features_exist)) cloudmask <- features_dl
    if (is.null(features_dl)) cloudmask <- features_exist
    if (! is.null(features_dl) & ! is.null(features_exist)) 
      cloudmask <- c(features_exist, features_dl)

    # get B2 asset as template for 10m band if needed
    if (resolution == 10 & asset_names == 'SCL') {
      asset_b2 <- 'B02' 
      features_b2 <- get_asset_terra(item = item_collection[[1]], 
                                     asset_names = asset_b2, 
                                     collection = collection, 
                                     aoi = aoi)
      cloudmask  <- lapply(X = cloudmask, 
                          FUN = terra::resample, 
                          y = features_b2[[1]], 
                          method = 'near')
    }
  }
  return(cloudmask)
}
