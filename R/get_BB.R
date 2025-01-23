#' This function gets coordinates of a bounding box defined by a vector (optional) and a raster
#'
#' @param path_raster character. path for raster file
#' @param aoi character. path for vector file
#' @param Buffer numeric. buffer applied to vector file (in meters)
#'
#' @return BB_XYcoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#' @export
get_BB <- function(path_raster, aoi = NULL, Buffer = 0){

  if (!is.null(aoi)){
    # get bounding box with a 50m buffer in order to allow for interpolation
    BB_XYcoords <- get_BB_from_Vector(path_raster = path_raster,
                                      aoi = aoi,
                                      Buffer = Buffer)
  } else if (is.null(aoi)){
    BB_XYcoords <- get_BB_from_fullImage(path_raster)
  }
  return(BB_XYcoords)
}


