#' downloads cloud mask
#'
#' @param collection_path path for colection previously saved
#' @param aoi geometry corresponding to one point
#' @param iChar plot ID
#' @param raster_dir directory where rasters are stored
#' @param overwrite boolean.
#' @param fraction_vegetation numeric. minimum fraction vegetation over plot
#' @param collection character. collection targeted with CDSE
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param nbCPU numeric. number of CPU
#' @param siteName character. name of the study site
#' @param crs_target numeric.
#'
#' @return list of cloudmasks
#' @importFrom terra rast values writeRaster
#' @export
#'
get_cloudmask <- function(collection_path, aoi, iChar, raster_dir, overwrite = F,
                          fraction_vegetation = 10, collection = "sentinel-2-l2a",
                          resolution = 10, nbCPU = 1, siteName = NULL, crs_target = NULL){

  # days of acquisition in ascending order
  cloudmasks <- NULL
  item_collection <- readRDS(file = collection_path)
  if (collection=='sentinel2-l2a-sen2lasrc'){
    item_collection <- item_collection |>
      rstactheia::items_sign_theia()
  }
  asset_names <- get_cloud_asset(item_collection = item_collection,
                                 collection = collection)
  suffix <- paste0('_',asset_names,'.tiff')
  Acqdates <- unique(rev(item_collection$acquisitionDate))
  if (length(Acqdates)>0){
    # clean raster directory if overwrite
    if (overwrite ==T)
      clean_rasters(raster_dir, Acqdates, iChar)
    # cloud mask: download / read if exists
    cloudmasks <- download_cloudmask(aoi = aoi, raster_dir = raster_dir,
                                     collection = collection,
                                     item_collection = item_collection, iChar = iChar,
                                     resolution = resolution, asset_names = asset_names,
                                     siteName = siteName, crs_target = crs_target)

    # check if sufficient vegetation fraction cover
    elim <- NULL
    for (dateAcq in names(cloudmasks)){
      if (is.null(siteName)) filename <- file.path(raster_dir,paste0('plot_',iChar,'_',dateAcq,suffix))
      if (!is.null(siteName)) filename <- file.path(raster_dir,paste0(siteName,'_',iChar,'_',dateAcq,suffix))
      total_pix <- length(which((terra::values(cloudmasks[[dateAcq]]))>0))
      # if using SCL allowing identifying vegetation pixels
      if (asset_names == 'SCL'){
        vegetation_pix <- length(which((terra::values(cloudmasks[[dateAcq]]))==4))
        if (total_pix>0) fcover <- 100*vegetation_pix/total_pix
        if (total_pix==0) fcover <- 0
        # if not, eliminate day and SCL
        if (fcover < fraction_vegetation){
          elim <- which(item_collection$acquisitionDate %in% dateAcq)
          item_collection$features <- item_collection$features[-elim]
          item_collection$acquisitionDate <- item_collection$acquisitionDate[-elim]
          cloudmasks <- cloudmasks[-which(names(cloudmasks) %in% dateAcq)]
        }
      }
    }
    if (!is.null(elim))
      saveRDS(object = item_collection, file = collection_path)
  }
  return(cloudmasks)
}
