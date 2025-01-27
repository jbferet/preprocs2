#' updates vegetation mask based on initial SCL and readiometric thresholds
#'
#' @param S2_item path for spectral bands required
#' @param acq date of acquisition
#' @param iChar plot ID
#' @param raster_dir directory where rasters are stored
#' @param cloudmask spatial raster
#' @param mask_path path for binary mask
#' @param aoiplot sf.
#' @param fraction_vegetation minimum fraction of vegetation required to keep the acquisition
#' @param RadiometricFilter list. criterions for cloud, shade, non vegetation
#' @param asset_cloud character. asset name for cloud mask
#' @param siteName character. name of the study site
#'
#' @return list of collections per plot
#' @importFrom terra rast writeRaster values
#' @export
#'
apply_radiometric_filter <- function(S2_item, acq, iChar, raster_dir,
                                     cloudmask, mask_path = NULL, aoiplot, 
                                     fraction_vegetation, siteName = NULL, 
                                     RadiometricFilter = NULL, asset_cloud){

  # define root path for output files
  if (is.null(siteName)) prefix <- file.path(raster_dir, paste0('plot_',iChar,'_',acq))
  if (!is.null(siteName)) prefix <- file.path(raster_dir, paste0(siteName,'_',iChar,'_',acq))
  bin_mask_file <- paste0(prefix, '_BIN.tiff')
  bin_mask_filtered <- paste0(prefix, '_BIN_v2.tiff')
  cloudmask_path <- paste0(prefix, '_',asset_cloud,'.tiff')

  # radiometric filter
  if (is.null(RadiometricFilter))
    RadiometricFilter <- list('cloudMask' = 500, 'shadeMask' = 1500, 'NDVIMask' = 0.65)

  validity <- T
  if (!file.exists(bin_mask_file) & !file.exists(bin_mask_filtered)){
    mainmask <- get_mainmask(mask_path, S2_item, aoiplot)
    ndvi <- (S2_item$B08-S2_item$B04)/(S2_item$B08+S2_item$B04)
    sel <- S2_item$B02 < RadiometricFilter$cloudMask &
      S2_item$B08 > RadiometricFilter$shadeMask &
      ndvi > RadiometricFilter$NDVIMask
    # get SCL
    if (asset_cloud == 'SCL') {
      bin_mask <- cloudmask
      targetVal <- 4
      bin_mask[which(!terra::values(cloudmask)==targetVal)] <- 0
      bin_mask[which(terra::values(cloudmask)==targetVal)] <- 1
    } else if (asset_cloud == 'CLM') {
      bin_mask <- 0*cloudmask
      selclear <- which(is.na(terra::values(cloudmask)))
      bin_mask[selclear] <- 1
    }
    mask_update <- bin_mask*sel*mainmask
    mask_init <- bin_mask*mainmask
    # get fCover from updated mask
    fCover <- 100*length(which(terra::values(mask_update)==1))/length(terra::values(mask_init)==1)
    # keep acquisition if fCover >fraction_vegetation
    if (fCover >fraction_vegetation){
      names(mask_update) <- 'binary mask update'
      names(bin_mask) <- 'binary mask'
      # save files
      terra::writeRaster(x = bin_mask, filename = bin_mask_file, overwrite = T)
      terra::writeRaster(x = mask_update, filename = bin_mask_filtered, overwrite = T)
      terra::writeRaster(x = cloudmask, filename = cloudmask_path, overwrite = T)
      validity <- T
    } else {
      # remove original SCL file
      if (file.exists(cloudmask_path)) unlink(x = cloudmask_path)
      validity <- F
    }
  }
  return(validity)
}
