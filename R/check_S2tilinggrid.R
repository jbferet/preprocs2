#' check if S2 tiling grid is available
#'
#' @param path_S2tilinggrid character. path for S2 tiling grid
#'
#' @return valid path for S2 tiling grid
#' @importFrom utils download.file unzip
#' @export

check_S2tilinggrid <- function(path_S2tilinggrid = NULL){
  if (is.null(path_S2tilinggrid)) 
    path_S2tilinggrid <- 'Sentinel-2_tiling_grid.kml'
  if (!file.exists(path_S2tilinggrid)){
    zip_S2tilinggrid <- 'Sentinel-2_tiling_grid.zip'
    url_S2tilinggrid <- 'https://sentiwiki.copernicus.eu/__attachments/1692737/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.zip?inst-v=d9cdf66c-96ab-4a2c-8d65-cc56a8f018b2'
    download.file(url = url_S2tilinggrid, 
                  destfile = zip_S2tilinggrid, method = 'auto', quiet = FALSE, mode = "wb")
    tileS2_kml <- file.path(dirname(path_S2tilinggrid),'tileS2_kml')
    unzip(zipfile = zip_S2tilinggrid,exdir = tileS2_kml)
    path_S2tilinggrid <- list.files(tileS2_kml,pattern = '.kml', full.names = T)
  }
  return(path_S2tilinggrid)
}
