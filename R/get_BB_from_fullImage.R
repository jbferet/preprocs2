#' This function gets extreme coordinates of a bounding box corresponding to a full image
#'
#' @param path_raster character. path for raster file
#'
#' @return BB_XYcoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#' @importFrom terra rast
#' @export
get_BB_from_fullImage <- function(path_raster){
  # get raster coordinates corresponding to Full image
  rasterobj <- terra::rast(path_raster)
  BB_XYcoords <- list()
  BB_XYcoords[['UL']] <- data.frame('row'=1,'col'=1)
  BB_XYcoords[['UR']] <- data.frame('row'=1,'col'=dim(rasterobj)[2])
  BB_XYcoords[['LL']] <- data.frame('row'=dim(rasterobj)[1],'col'=1)
  BB_XYcoords[['LR']] <- data.frame('row'=dim(rasterobj)[1],'col'=dim(rasterobj)[2])
  return(BB_XYcoords)
}
