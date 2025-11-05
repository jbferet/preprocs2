#' downloads a time series defined by item collection
#'
#' @param collection_path character. directory where plot collection is stored
#' @param aoi geometry corresponding to one point
#' @param iChar plot ID
#' @param raster_dir directory where rasters are stored
#' @param mask_path path for binary mask
#' @param fraction_vegetation numeric. minimum fraction vegetation over plot
#' @param stac_info list
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param offset numeric. offset value
#' @param offset_B2 boolean.
#' @param corr_BRF boolean.
#' @param p list.
#' @param RadiometricFilter list. values for radiometric filter shade cloud vegetation
#' @param overwrite boolean.
#' @param siteName character. name of the study site
#' @param rast_out boolean. should S2 SpatRaster be obtained as output?
#' @param additional_process additional process to be applied to S2_items once downloaded
#' @param crs_target numeric.
#' @param original_clouds boolean
#' @param argsin list
#' @param writeoutput boolean. should output file be saved?
#' @param bands2correct character. name of bands to correct from geometry
#'
#'
#' @return list of collections per plot
#' @export
#'
download_s2_collection <- function(collection_path, aoi, iChar, raster_dir,
                                  mask_path = NULL, fraction_vegetation = 5,
                                  stac_info, resolution = 10,
                                  offset = 1000, offset_B2 = F, corr_BRF = F,
                                  p = NULL, RadiometricFilter = NULL,
                                  overwrite = T, siteName = NULL, rast_out = T,
                                  additional_process = NULL, crs_target = NULL,
                                  original_clouds = TRUE, argsin = NULL,
                                  writeoutput = T,
                                  bands2correct = c('B8A', 'B11', 'B12')){
  # get collection cloud masks
  cloudmasks <- get_cloudmask(collection_path = collection_path, aoi = aoi,
                              iChar = iChar, raster_dir = raster_dir,
                              overwrite = overwrite, siteName = siteName,
                              fraction_vegetation = fraction_vegetation,
                              stac_info = stac_info, resolution = resolution,
                              crs_target = crs_target)

  # update collection cloud masks
  S2data <- update_mask(aoi = aoi, collection_path = collection_path,
                        iChar = iChar, raster_dir = raster_dir,
                        mask_path = mask_path, cloudmasks = cloudmasks,
                        fraction_vegetation = fraction_vegetation,
                        siteName = siteName, offset = offset,
                        stac_info = stac_info, resolution = resolution,
                        RadiometricFilter = RadiometricFilter,
                        overwrite = overwrite, crs_target = crs_target,
                        original_clouds = original_clouds,
                        writeoutput = writeoutput)

  # download S2 collection
  S2_items <- NULL
  if (!is.null(S2data$S2_items)){
    S2_items <- download_s2(aoi = aoi, raster_dir = raster_dir, iChar = iChar,
                            stac_info = stac_info,
                            collection_path = collection_path,
                            S2_items = S2data$S2_items, resolution = resolution,
                            offset = offset, offset_B2 = offset_B2,
                            corr_BRF = corr_BRF, siteName = siteName,
                            crs_target = crs_target, writeoutput = writeoutput,
                            bands2correct = bands2correct,
                            overwrite = overwrite)

    # if (corr_BRF | offset_B2){
    #   S2data <- update_mask(aoi = aoi, collection_path = collection_path,
    #                         iChar = iChar, raster_dir = raster_dir,
    #                         mask_path = mask_path, cloudmasks = cloudmasks,
    #                         fraction_vegetation = fraction_vegetation,
    #                         siteName = siteName, offset = offset,
    #                         collection = collection, resolution = resolution,
    #                         RadiometricFilter = RadiometricFilter,
    #                         overwrite = T, crs_target = crs_target,
    #                         original_clouds = original_clouds,
    #                         S2_items = S2_items)
    # }

    if (!is.null(additional_process)){
      argsin$iChar <- iChar
      res_add <- additional_process(S2_refl = S2_items,
                                    S2_mask = S2data$mask_update,
                                    argsin = argsin)
    }
  }
  if (!is.null(p)) p()
  if (!rast_out)
    S2_items <- NULL
  return(S2_items)
}
