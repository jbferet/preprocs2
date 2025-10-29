#' downloads S2 reflectance data
#'
#' @param aoi geometry corresponding to one point
#' @param raster_dir directory where rasters are stored
#' @param collection_path path for collection to download
#' @param iChar plot ID
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param S2_items list. S2 items already downloaded
#' @param offset numeric. S2 offset value
#' @param offset_B2 boolean.
#' @param stac_info list.
#' @param corr_BRF boolean.
#' @param siteName character. name of the study site
#' @param crs_target numeric.
#' @param writeoutput boolean. should output file be saved?
#' @param bands2correct character. name of bands to correct from geometry
#'
#' @return list of collections per plot
#' @importFrom terra rast
#' @importFrom sf st_transform st_bbox st_crs
#' @importFrom rstac assets_url
#' @export
#'
download_s2 <- function(aoi, raster_dir, collection_path, iChar, resolution,
                        S2_items = NULL, offset = 1000, offset_B2 = F,
                        stac_info, corr_BRF = F,
                        siteName = NULL, crs_target = NULL, writeoutput = T,
                        bands2correct = c('B8A', 'B11', 'B12')){

  item_collection <- readRDS(file = collection_path)
  asset_cloud <- get_cloud_asset(stac_info = stac_info)
  baseline <- get_s2_baseline(stac_info, item_collection)

  # get bounding box
  asset_names <- c('B02', 'B03', 'B04', 'B05', 'B06',
                   'B07', 'B08', 'B8A', 'B11', 'B12')
  asset_names_list <- list_assets(S2_items = S2_items,
                                  asset_names = asset_names,
                                  dateAcqs = item_collection$acquisitionDate)

  S2_items_update <- mapply(FUN = get_asset_terra,
                            item = item_collection$features,
                            asset_names = asset_names_list,
                            MoreArgs = list(collection = stac_info$collection,
                                            aoi = aoi,
                                            crs_target = crs_target),
                            SIMPLIFY = F)
  names(S2_items_update) <- item_collection$acquisitionDate

  for (i in seq_len(length(baseline))){
    if (as.numeric(baseline[[i]])>=4 & asset_cloud == 'SCL')
      S2_items_update[[i]] <- lapply(X = S2_items_update[[i]],
                                     FUN = function(x, offset){x - offset},
                                     offset)
  }
  S2_items_final <- list()
  for (dateAcq in as.character(item_collection$acquisitionDate)){
    if (is.null(S2_items[[dateAcq]]))
      S2_items_final[[dateAcq]] <- S2_items_update[[dateAcq]]
    if (is.null(S2_items_update[[dateAcq]]))
      S2_items_final[[dateAcq]] <- S2_items[[dateAcq]]
    if (! is.null(S2_items_update[[dateAcq]]) & ! is.null(S2_items_update[[dateAcq]]))
      S2_items_final[[dateAcq]] <- c(S2_items[[dateAcq]], S2_items_update[[dateAcq]])
    S2_items_final[[dateAcq]] <- S2_items_final[[dateAcq]][asset_names]
  }

  s2_items <- list()
  for (i in seq_len(length(item_collection$acquisitionDate))){
    resitems <- lapply(lapply(S2_items_final[[i]], terra::res), '[[', 1)
    # harmonize spatial resolution
    template_Rast <- S2_items_final[[i]][which(round(unlist(resitems))==resolution)[1]]
    S2_items_final[[i]]  <- lapply(X = S2_items_final[[i]],
                                   FUN = terra::resample,
                                   template_Rast[[1]], method = 'near')
    acq <- as.character(item_collection$acquisitionDate[[i]])
    s2_items[[acq]] <- correct_s2_stack(s2_items = S2_items_final[[i]],
                                        acq = acq, raster_dir = raster_dir,
                                        aoi = aoi, offset_B2 = offset_B2,
                                        corr_BRF = corr_BRF,
                                        bands2correct = bands2correct)
    # save reflectance file
    if (writeoutput){
      if (is.null(siteName))
        filename <- file.path(raster_dir, paste0('plot_',iChar,'_',
                                                 acq, '.tiff'))
      if (!is.null(siteName))
        filename <- file.path(raster_dir, paste0(siteName,'_',iChar,'_',
                                                 acq, '.tiff'))
      terra::writeRaster(x = s2_items[[acq]], filename = filename, overwrite = T)
    }
  }
  return(s2_items)
}
