#' downloads individual acquisitions
#'
#' @param acq date.
#' @param band_url list.
#' @param raster_dir character.
#' @param plots_bbox sf.
#' @param collection character.
#' @param iChar character.
#' @param asset_names list.
#' @param skipExists boolean.
#'
#' @return out_file
#' @export
#'
download_s2_acq <- function(acq, band_url, raster_dir, plots_bbox,
                            collection = 'sentinel-2-l2a', iChar, asset_names,
                            skipExists = T){

  out_fileStack <-  file.path(raster_dir,paste0('plot_',iChar,'_',acq, '.tiff'))
  alreadyexistsStack <- file.exists(out_fileStack)
  if (skipExists){
    out_file <- NULL
  } else {
    out_file <-  as.list(file.path(raster_dir,paste0('plot_',iChar,'_',acq, '_',asset_names,'.tiff')))
  }
  # if the final stack does not exist
  if (! alreadyexistsStack){
    out_file <-  as.list(file.path(raster_dir,paste0('plot_',iChar,'_',acq, '_',asset_names,'.tiff')))
    alreadyexistsBands <- file.exists(unlist(out_file))
    # names(alreadyexistsBands) <- asset_names
    # existing bands are not dowonloaded
    out_file_dl <- out_file
    elim <- c()
    if (skipExists & collection == 'sentinel-2-l2a'){
      asset_names2 <- c('B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12')
    } else if (skipExists & collection == 'sentinel2-l2a-sen2lasrc'){
      asset_names2 <- c('band2.tif', 'band3.tif', 'band4.tif', 'band5.tif',
                        'band6.tif', 'band7.tif', 'band8.tif', 'band8a.tif', 'band11.tif', 'band12.tif')
    } else if (!skipExists){
      asset_names2 <- asset_names
    }
    names(alreadyexistsBands) <- asset_names2
    if (TRUE %in% alreadyexistsBands){
      for (asset in asset_names2){
        if (alreadyexistsBands[asset]) elim <- c(elim, grep(pattern = asset, x = band_url))
      }
      band_url <- band_url[-elim]
      out_file_dl <- out_file_dl[-which(alreadyexistsBands)]
    }
    # if (collection=='sentinel-2-l2a'){
    #   band_curl <- as.list(make_vsicurl_url(band_url, collection = collection))
    # } else if (collection=='sentinel2-l2a-sen2lasrc'){
    #   band_curl <- as.list(make_vsicurl_theia_url(band_url))
    # }
    band_curl <- as.list(band_url)
    mapply(FUN = get_asset,
           asset_url = band_curl, out_file = out_file_dl,
           MoreArgs = list(plots_bbox = plots_bbox))
  }
  return(unlist(out_file))
}

