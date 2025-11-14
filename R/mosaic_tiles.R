#' compute spectral indices and composite for a given period of time
#'
#' @param pattern character. identify which files to mosaic
#' @param dir_path character. firectory where rasters to mosaic are stored
#' @param vrt_save character. where to save vrt
#' @param site_name character. name of the site
#' @param overwrite boolean
#'
#' @return tif_path
#' @importFrom terra vrt
#' @importFrom sf gdal_utils
#' @export
#'
mosaic_tiles <- function(pattern, dir_path, vrt_save, site_name, overwrite = FALSE){
  # create vrt
  listfiles <- list.files(dir_path, pattern = pattern, full.names = TRUE)
  output_vrt_path <- file.path(getwd(), paste0(site_name, '_', pattern,'_mosaic.vrt'))
  if (!file.exists(output_vrt_path))
    v <- terra::vrt(x = listfiles, filename = output_vrt_path)
  # create tiff from vrt
  tif_path <- file.path(dir_path, paste0('_', site_name, '_', pattern,'_mosaic.tiff'))
  if (!file.exists(tif_path) | overwrite)
    sf::gdal_utils(util = 'translate', source = output_vrt_path,
                   destination = tif_path, options = c("-co", "COMPRESS=LZW"))
  # create tiff from vrt
  dir.create(path = vrt_save, showWarnings = FALSE, recursive = TRUE)
  output_vrt_path2 <- file.path(vrt_save, paste0(site_name, '_', pattern,'_mosaic.vrt'))
  file.rename(from = output_vrt_path, to = output_vrt_path2)
  return(tif_path)
}
