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
#'
#' @return list of collections per plot
#' @importFrom terra rast values writeRaster
#' @export
#'
get_cloudmask <- function(collection_path, aoi, iChar, raster_dir, overwrite = F,
                          fraction_vegetation = 10, collection = "sentinel-2-l2a",
                          resolution = 10, nbCPU = 1){

  # days of acquisition in ascending order
  item_collection <- readRDS(file = collection_path)
  if (collection=='sentinel2-l2a-sen2lasrc'){
    item_collection <- item_collection |>
      rstactheia::items_sign_theia()
  # } else if (collection=='sentinel-2-l2a'){
  #   item_collection <- item_collection |>
  #     rstac::items_sign(
  #       rstac::sign_planetary_computer()
  #     )
  }
  asset_names <- get_cloud_asset(item_collection, collection)
  suffix <- paste0('_',asset_names,'.tiff')
  Acqdates <- unique(rev(item_collection$acquisitionDate))
  # clean raster directory if overwrite
  if (overwrite ==T) clean_rasters(raster_dir, Acqdates, iChar)
  # check cloud masks: are there already downloaded ones, are there missing ones?
  cloud_status <- list_cloud_dl(raster_dir, Acqdates, iChar, asset_names = asset_names)
  # download SCL required
  cloud_info <- download_cloudmask(aoi = aoi, raster_dir = raster_dir,
                                   collection = collection,
                                   collection_info = item_collection, iChar = iChar,
                                   resolution = resolution, asset_names = asset_names)
  # get cloud data and directory where stored
  cloud_dl <- cloud_info$cloud_dl
  out_dir <- cloud_info$out_dir

  # combine list of SCL already downloaded and SCL to download
  SCL <- list()
  for (d in Acqdates){
    dd <- as.character(as.Date(d))
    if (dd %in% cloud_status$days_dl) SCL[[as.character(as.Date(d))]] <- cloud_dl[[dd]]
    if (dd %in% cloud_status$days_ok){
      SCL_path <- file.path(raster_dir,paste0('plot_',iChar,'_',as.Date(d), suffix))
      SCL[[dd]] <- terra::rast(SCL_path)
    }
  }
  # check if sufficient vegetation fraction cover
  elim <- NULL
  for (d in as.Date(Acqdates)){
    dd <- as.character(as.Date(d))
    filename <- file.path(raster_dir,paste0('plot_',iChar,'_',dd, suffix))
    total_pix <- length(which((terra::values(SCL[[dd]]))>0))
    # if using SCL allowing identifying vegetation pixels
    if (asset_names == 'SCL'){
      vegetation_pix <- length(which((terra::values(SCL[[dd]]))==4))
      if (total_pix>0) fcover <- 100*vegetation_pix/total_pix
      if (total_pix==0) fcover <- 0
      # if not, eliminate day and SCL
      if (fcover < fraction_vegetation){
        elim <- which(item_collection$acquisitionDate %in% dd)
        item_collection$features <- item_collection$features[-elim]
        item_collection$acquisitionDate <- item_collection$acquisitionDate[-elim]
        SCL <- SCL[-which(names(SCL) %in% as.Date(d))]
        if (file.exists(filename)) unlink(filename)
      } else if  (fcover > fraction_vegetation){
        if (!file.exists(filename) | overwrite == T)
          terra::writeRaster(x = SCL[[dd]], filename = filename, overwrite = overwrite)
      }
    } else if (asset_names == 'CLM'){
      if (!file.exists(filename) | overwrite == T)
        terra::writeRaster(x = SCL[[dd]], filename = filename, overwrite = overwrite)
    }
  }
  if (!is.null(elim)) saveRDS(object = item_collection, file = collection_path)
  if (!is.null(out_dir)){
    if (dir.exists(out_dir)) unlink(x = out_dir, recursive = T, force = T)
  }
  # rm(list=ls())
  gc()
  return()
}
