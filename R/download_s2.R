#' downloads S2 reflectance data
#'
#' @param aoi geometry corresponding to one point
#' @param raster_dir directory where rasters are stored
#' @param collection_path path for collection to download
#' @param iChar plot ID
#' @param resolution numeric. spatial resolution (10 or 20)
#' @param offset numeric. S2 offset value
#' @param offset_B2 boolean.
#' @param collection character.
#' @param corr_BRF boolean.
#' @param clean_bands boolean.
#'
#' @return list of collections per plot
#' @importFrom terra rast writeRaster
#' @importFrom sf st_transform st_bbox st_crs
#' @importFrom rstac assets_url
#' @export
#'
download_s2 <- function(aoi, raster_dir, collection_path, iChar, resolution,
                        offset = 1000, offset_B2 = F, collection = 'sentinel-2-l2a',
                        corr_BRF = F, clean_bands = T){

  collection_info <- readRDS(file = collection_path)
  if (collection=='sentinel2-l2a-sen2lasrc'){
    collection_info <- collection_info |>
      rstactheia::items_sign_theia()
  } else if (collection=='sentinel-2-l2a'){
    collection_info <- collection_info |>
      rstac::items_sign(
        rstac::sign_planetary_computer()
      )
  }

  # get bounding box
  plots_bbox <- aoi |>
    sf::st_transform(sf::st_crs(4326)) |>
    sf::st_bbox()
  # get all SCL assets
  asset_names <- c('B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12')
  band_url0 <- rstac::assets_url(collection_info, asset_names = asset_names)
  if (!is.null(band_url0)){
    # get B2 asset as template for 10m band if needed
    if (collection == 'sentinel-2-l2a'){
      if (resolution == 10) asset_tmp <- 'B02'
      if (resolution == 20) asset_tmp <- 'B05'
      btmp_url <- rstac::assets_url(collection_info, asset_names = asset_tmp)
      btmp_url <- make_vsicurl_url(btmp_url, collection = collection)[1]
      out_file <- tempfile()
      get_asset(asset_url = btmp_url, out_file = out_file, plots_bbox = plots_bbox)
      template_Rast <- terra::rast(out_file)
    } else {
      template_Rast <- NULL
    }
    # get bands per acquisition
    selAcq <- list()
    for (dateacq in collection_info$acquisitionDate){
      dateacq2 <- format(as.Date(dateacq),format = '%Y%m%d')
      if (collection == 'sentinel-2-l2a'){
        sel <- which(stringr::str_detect(string = band_url0, pattern = paste0('MSIL2A_',dateacq2)))
        asset_names2 <- c('B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12')
      } else {
        sel <- which(stringr::str_detect(string = band_url0, pattern = paste0('MSIL2A-SEN2LASRC_',dateacq2)))
        asset_names2 <- c('band2.tif', 'band3.tif', 'band4.tif', 'band5.tif',
                          'band6.tif', 'band7.tif', 'band8.tif', 'band8a.tif', 'band11.tif', 'band12.tif')
      }
      selAcq[[as.character(as.Date(dateacq))]] <- band_url0[sel]
      # check band order
      selAcq[[as.character(as.Date(dateacq))]] <- check_order_bands(acq = selAcq[[as.character(as.Date(dateacq))]],
                                                                    patterns = asset_names2)
    }
    if (collection=='sentinel-2-l2a'){
    	band_url <- lapply(X = selAcq, make_vsicurl_url, collection = collection)
    } else if (collection=='sentinel2-l2a-sen2lasrc'){
      band_url <- lapply(X = selAcq, make_vsicurl_theia_url)
    }
    # band_url <- lapply(X = selAcq, make_vsicurl_url, collection = collection)

    if (collection == 'sentinel-2-l2a'){
      baseline <- lapply(lapply(collection_info$features,'[[','properties'),
                         '[[', 's2:processing_baseline')
    } else if (collection == 'sentinel2-l2a-sen2lasrc'){
      baseline <- lapply(lapply(lapply(collection_info$features,'[[','properties'),
                                '[[', 'processing:software'),
                         '[[', 'sen2lasrc')
    }
    s2_files <- mapply(FUN = download_s2_acq,
                       acq = as.list(collection_info$acquisitionDate),
                       band_url = band_url,
                       MoreArgs = list(raster_dir = raster_dir,
                                       plots_bbox = plots_bbox,
                                       iChar = iChar,
                                       asset_names = asset_names,
                                       collection = collection,
                                       skipExists = T),
                       SIMPLIFY = F)
    sel <- which(!unlist(lapply(X = s2_files, FUN = is.null)))
    if (length(sel)>0){
      s2_items <- s2_files[sel]
      acq <- as.list(collection_info$acquisitionDate)[sel]
      baseline_list <- as.list(baseline[sel])
      for (ii in seq_len(length(sel))){
        correct_s2stack(s2_items = s2_items[[ii]], acq = acq[[ii]],
                        raster_dir = raster_dir,
                        baseline = baseline_list[[ii]],
                        template_Rast = template_Rast,
                        asset_names = asset_names, iChar = iChar,
                        aoi = aoi, offset = offset, offset_B2 = offset_B2,
                        corr_BRF = corr_BRF, clean_bands = clean_bands)

      }
    }
  }
  return(invisible())
}
