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
#' @param radiometric_filter list. criterions for cloud, shade, non vegetation
#' @param asset_cloud character. asset name for cloud mask
#' @param site_name character. name of the study site
#' @param original_clouds boolean
#' @param overwrite boolean
#' @param writeoutput boolean. should output file be saved?
#'
#' @return list of updated masks and valid dates of acquisition
#' @importFrom terra rast writeRaster values
#' @export
#'
apply_radiometric_filter <- function(S2_item, acq, iChar, raster_dir,
                                     cloudmask, mask_path = NULL, aoiplot,
                                     fraction_vegetation, site_name = NULL,
                                     radiometric_filter = NULL, asset_cloud,
                                     original_clouds = TRUE, overwrite = TRUE,
                                     writeoutput = TRUE){

  # define root path for output files
  if (is.null(site_name))
    prefix <- file.path(raster_dir, paste0('plot_',iChar,'_',acq))
  if (!is.null(site_name))
    prefix <- file.path(raster_dir, paste0(site_name,'_',iChar,'_',acq))
  bin_mask_file <- paste0(prefix, '_BIN.tiff')
  bin_mask_filtered <- paste0(prefix, '_BIN_v2.tiff')
  cloudmask_path <- paste0(prefix, '_',asset_cloud,'.tiff')
  mask_update <- NULL
  # radiometric filter
  if (is.null(radiometric_filter))
    radiometric_filter <- list('cloudMask' = 350,
                              'shadeMask' = 1500,
                              'NDVIMask' = 0.65)
  validity <- TRUE
  if (overwrite | (!file.exists(bin_mask_file) & !file.exists(bin_mask_filtered))){
    mainmask <- get_mainmask(mask_path = mask_path,
                             S2_dl = S2_item,
                             aoiplot = aoiplot)
    ndvi <- (S2_item$B08-S2_item$B04)/(S2_item$B08+S2_item$B04)
    sel <- S2_item$B02 < radiometric_filter$cloudMask &
      S2_item$B08 > radiometric_filter$shadeMask &
      ndvi > radiometric_filter$NDVIMask
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
    } else if (asset_cloud == 'CLM_R1') {
      bin_mask <- 0*cloudmask
      selclear <- which(terra::values(cloudmask)==0)
      bin_mask[selclear] <- 1
    }
    if (!original_clouds)
      bin_mask <- 0*bin_mask+1

    mask_update <- bin_mask*sel*mainmask
    mask_init <- bin_mask*mainmask
    # get fCover from updated mask
    fCover <- 100*length(which(terra::values(mask_update)==1))/length(terra::values(mask_init)==1)
    # keep acquisition if fCover >fraction_vegetation
    if (fCover >fraction_vegetation){
      names(mask_update) <- 'binary mask update'
      names(bin_mask) <- 'binary mask'
      # save files
      if (writeoutput){
        terra::writeRaster(x = bin_mask, filename = bin_mask_file,
                           overwrite = TRUE)
        terra::writeRaster(x = mask_update, filename = bin_mask_filtered,
                           overwrite = TRUE)
        terra::writeRaster(x = cloudmask, filename = cloudmask_path,
                           overwrite = TRUE)
      }
      validity <- TRUE
    } else {
      # remove original SCL file
      if (file.exists(cloudmask_path)) unlink(x = cloudmask_path)
      mask_update <- NULL
      validity <- FALSE
    }
  }
  updatedMasks <- list('validity' = validity, 'mask_update' = mask_update)
  return(updatedMasks)
}
