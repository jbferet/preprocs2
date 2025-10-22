#' check if S2 tiling grid is available
#'
#' @param path_S2tilinggrid character. path for S2 tiling grid
#'
#' @return valid path for S2 tiling grid
#' @importFrom utils download.file unzip
#' @importFrom curl has_internet
#' @export

check_s2_tiling_grid <- function(path_S2tilinggrid = NULL){
  if (is.null(path_S2tilinggrid))
    path_S2tilinggrid <- 'Sentinel-2_tiling_grid.kml'
  if (!file.exists(path_S2tilinggrid)){
    message('Sentinel-2 tiling grid not available')
    message('will be automatically downloaded and saved at this location')
    print(path_S2tilinggrid)
    zip_S2tilinggrid <- 'Sentinel-2_tiling_grid.zip'
    url_S2tilinggrid <- 'https://sentiwiki.copernicus.eu/__attachments/1692737/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.zip?inst-v=d9cdf66c-96ab-4a2c-8d65-cc56a8f018b2'
    if (curl::has_internet()){
      utils::download.file(url = url_S2tilinggrid,
                    destfile = zip_S2tilinggrid, method = 'auto', quiet = FALSE, mode = "wb")
      tileS2_kml <- file.path(dirname(path_S2tilinggrid),'tileS2_kml')
      utils::unzip(zipfile = zip_S2tilinggrid,exdir = tileS2_kml)
      path_S2tilinggrid_tmp <- list.files(tileS2_kml,pattern = '.kml', full.names = T)
      file.rename(from = path_S2tilinggrid_tmp, to = path_S2tilinggrid)
      file.remove(zip_S2tilinggrid)
      unlink(x = dirname(path_S2tilinggrid_tmp), force = T, recursive = T)
    } else {
      message('No internet connexion available. Process will stop')
      message('please make sure you get interent access to run preprocS2')
      stop_quietly()
    }
  }
  return(path_S2tilinggrid)
}
