#' downloads individual acquisitions
#'
#' @param acq date.
#' @param band_url list.
#' @param raster_dir character.
#' @param plots_bbox sf.
#' @param iChar character.
#' @param asset_names list.
#' @param skipExists boolean.
#'
#' @return out_file
#' @export
#'
download_s2_acq <- function(acq, band_url, raster_dir, plots_bbox,
                            iChar, asset_names, skipExists = T){

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
    names(alreadyexistsBands) <- asset_names
    # existing bands are not dowonloaded
    out_file_dl <- out_file
    elim <- c()
    if (TRUE %in% alreadyexistsBands){
      for (asset in asset_names){
        if (alreadyexistsBands[asset]) elim <- c(elim, grep(pattern = asset, x = band_url))
      }
      band_url <- band_url[-elim]
      out_file_dl <- out_file_dl[-which(alreadyexistsBands)]
    }
    band_curl <- as.list(make_vsicurl_url(band_url))
    mapply(FUN = get_asset,
           asset_url = band_curl, out_file = out_file_dl,
           MoreArgs = list(plots_bbox = plots_bbox))
  }
  return(unlist(out_file))
}

