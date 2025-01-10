#' downloads S2 reflectance to update the vegetation mask
#'
#' @param raster_dir directory where rasters are stored
#' @param mask_path directory where rasters are stored
#' @param collection_path path for collection to download
#' @param iChar plot ID
#' @param aoi geometry corresponding to one point
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param collection character.
#' @param fraction_vegetation numeric. minimum % of vegetatiomn to have on plot footprint
#' @param offset numeric. value of S2 offset
#' @param RadiometricFilter list. list of radiometric filters for shade, clouds and vegetation
#'
#' @return list of collections per plot
#' @importFrom terra writeRaster values
#' @importFrom sf st_transform st_bbox
#' @importFrom rstac assets_url
#' @export
#'
get_B248_filter <- function(raster_dir, mask_path, collection_path,
                            iChar, aoi, resolution, collection = 'sentinel-2-l2a',
                            fraction_vegetation, offset = 1000,
                            RadiometricFilter = NULL){
  # read collection
  item_collection <- readRDS(file = collection_path)
  asset_cloud <- get_cloud_asset(item_collection, collection)
  suffix <- paste0('_',asset_cloud,'.tiff')
  # get bounding box
  plots_bbox <- aoi |>
    sf::st_transform(sf::st_crs(4326)) |>
    sf::st_bbox()
  # get assets
  asset_names <- c('B02', 'B04', 'B08')
  cloudmask <- keepit <- acq2keep <- NULL
  band_url0 <- rstac::assets_url(item_collection, asset_names = asset_names)
  # if bands available
  if (!is.null(band_url0)){
    rootname <- file.path(raster_dir, paste0('plot_',iChar,'_'))
    updatemask <- check_existing_mask(item_collection = item_collection,
                                      band_url = band_url0,
                                      rootname = rootname)
    collec_dl <- updatemask$collec_dl
    # if some masks still need to be updated
    if (length(updatemask$selAcq)>0){
      band_url <- lapply(X = updatemask$selAcq, make_vsicurl_url, collection = collection)
      S2_items <- mapply(FUN = download_s2_acq,
                        acq = as.list(collec_dl$acquisitionDate),
                        band_url = band_url,
                        MoreArgs = list(raster_dir = raster_dir,
                                        plots_bbox = plots_bbox,
                                        iChar = iChar,
                                        asset_names = asset_names,
                                        skipExists = F),
                        SIMPLIFY = F)
      whichAcq2dl <- which(!unlist(lapply(X = S2_items, FUN = is.null)))
      if (length(whichAcq2dl)>0){
        baseline <- lapply(lapply(collec_dl$features,'[[','properties'),
                           '[[', 's2:processing_baseline')
        validity <- mapply(FUN = apply_radiometric_filter,
                           S2_items = S2_items[whichAcq2dl],
                           acq = as.list(collec_dl$acquisitionDate)[whichAcq2dl],
                           baseline = baseline[whichAcq2dl],
                           MoreArgs = list(raster_dir = raster_dir,
                                           mask_path = mask_path,
                                           iChar = iChar,
                                           aoiplot = aoi,
                                           fraction_vegetation = fraction_vegetation,
                                           offset = offset,
                                           RadiometricFilter = RadiometricFilter,
                                           asset_cloud = asset_cloud),
                           SIMPLIFY = F)
        keepit <- which(unlist(validity))
        if (length(keepit)>0) acq2keep <- collec_dl$acquisitionDate[keepit]
      }
    }
    if (!is.null(acq2keep)) {
      selacq <- match(acq2keep, updatemask$df_acq$acq)
      updatemask$df_acq$already[selacq] <- T
      keepit2 <- which(updatemask$df_acq$already)
      item_collection$acquisitionDate <- item_collection$acquisitionDate[keepit2]
      item_collection$features <- item_collection$features[keepit2]
      saveRDS(object = item_collection, file = collection_path)
      cloud_file <-  as.list(file.path(raster_dir,
                                       paste0('plot_', iChar, '_',
                                              item_collection$acquisitionDate, suffix)))
      cloudmask <- lapply(X = cloud_file, FUN = terra::rast)
      names(cloudmask) <- as.Date(item_collection$acquisitionDate)
    } else {
      keepit2 <- which(updatemask$df_acq$already)
      item_collection$acquisitionDate <- item_collection$acquisitionDate[keepit2]
      item_collection$features <- item_collection$features[keepit2]
      saveRDS(object = item_collection, file = collection_path)
    }
  }
  return(list('cloudmask'= cloudmask, 'collection_info' = item_collection))
}
