#' downloads SCL from Planetary
#'
#' @param aoi geometry corresponding to one point
#' @param raster_dir directory where rasters are stored
#' @param collection_info description of collection to download
#' @param iChar plot ID
#' @param collection character. name of the collection
#' @param asset_names character.
#' @param resolution numeric. spatial resolution (10 or 20)
#'
#' @return list of collections per plot
#' @importFrom rstac assets_url
#' @importFrom terra rast resample
#' @importFrom sf st_transform st_bbox
#' @export
#'
download_cloudmask <- function(aoi, raster_dir, collection_info, iChar,
                               collection = 'sentinel-2-l2a', asset_names,
                               resolution){
  
  cloud_dl <- out_dir <- NULL
  # if collection not empty
  if (length(collection_info$acquisitionDate)>0){
    # stac_source <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    # get bounding box
    plots_bbox <- aoi |>
      sf::st_transform(4326) |>
      sf::st_bbox()
    # get all SCL assets
    band_url <- rstac::assets_url(items = collection_info, asset_names = asset_names)
    dateAcq <- get_dateAcq(band_url = band_url, collection = collection)
    if (collection == 'sentinel-2-l2a'){
      band_url <- as.list(make_vsicurl_url(band_url, collection = collection))
    } else if (collection == 'sentinel2-l2a-sen2lasrc'){
      band_url <- as.list(make_vsicurl_theia_url(band_url))
    }
    # define file name for cloud mask
    filenames <- basename(paste0('plot_',iChar,'_',dateAcq,'_',asset_names,'.tiff'))
    filepath <- file.path(raster_dir,filenames)
    # check if expected files already exist
    exist_file <- file.exists(filepath)
    # define temporary directory for plot SCL
    out_dir <- file.path(raster_dir,paste0('tmp_',asset_names,'_',iChar))
    filepath_tmp <- file.path(out_dir,filenames)
    filepath_consolidate <- filepath
    # if cloud file does not exists
    if (FALSE %in% exist_file){
      dir.create(out_dir, showWarnings = F, recursive = T)
      # keep downloading these files
      keep_dl <- which(exist_file==FALSE)
      # corresponding acquisition date
      keep_dateAcq <- dateAcq[keep_dl]
      out_file <-  as.list(file.path(out_dir,filenames[keep_dl]))
      band_url <- band_url[keep_dl]
      # download
      mapply(FUN = get_asset, asset_url = band_url, out_file = out_file,
             MoreArgs = list(plots_bbox = plots_bbox))
      filepath_consolidate[keep_dl] <- out_file
    }
    cloud_dl <- lapply(X = filepath_consolidate, FUN = terra::rast)
    # get B2 asset as template for 10m band if needed
    if (resolution == 10 & asset_names == 'SCL') {
      asset_b2 <- 'B02'
      b02_url <- rstac::assets_url(items = collection_info, asset_names = asset_b2)
      b02_url <- make_vsicurl_url(b02_url, collection = collection)[1]
      out_file <- tempfile()
      get_asset(asset_url = b02_url, out_file = out_file, plots_bbox = plots_bbox)
      temp_10m <- terra::rast(out_file)
      cloud_dl  <- lapply(X = cloud_dl, FUN = terra::resample, temp_10m, method = 'near')
    } 
    names(cloud_dl) <- as.Date(dateAcq)
  }
  return(list('cloud_dl' = cloud_dl, 'out_dir' = out_dir))
}
