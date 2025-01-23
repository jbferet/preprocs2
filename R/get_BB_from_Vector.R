#' This gets bounding box corresponding to a vector from a raster (UL, UR, LL, LR corners)
#'
#' @param path_raster character. path for raster file
#' @param aoi sf object.
#' @param Buffer numeric. buffer applied to vector file (in meters)
#'
#' @return BB_XYcoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#' @importFrom methods as
#' @importFrom terra extract rast vect ext buffer intersect
#' @export
#'
get_BB_from_Vector <- function(path_raster, aoi, Buffer = 0){

  Raster <- terra::rast(path_raster)
  # extract BB coordinates from vector
  aoi_spatvect <- as(aoi,'SpatVector')
  BB_vector <- terra::buffer(x = aoi_spatvect, Buffer)
  # # extract BB coordinates from raster
  BB_raster <- terra::ext(x = terra::rast((path_raster)))
  # # compute intersection
  Intersect <- terra::intersect(x = BB_raster, y = BB_vector)
  xmin <- Intersect[1]
  xmax <- Intersect[2]
  ymin <- Intersect[3]
  ymax <- Intersect[4]

  # get coordinates of bounding box corresponding to vector
  Corners <- list('UR' = terra::vect(x = cbind(xmax, ymax)),
                  'LR' = terra::vect(x = cbind(xmax, ymin)),
                  'UL' = terra::vect(x = cbind(xmin, ymax)),
                  'LL' = terra::vect(x = cbind(xmin, ymin)))
  for (corner in names(Corners)) terra::crs(Corners[[corner]]) <- terra::crs(x = aoi_spatvect)

  # get coordinates for corners of bounding box
  BB_XYcoords <- list()
  for (corner in names(Corners)){
    ex.df <- as.data.frame(terra::extract(x = Raster,
                                          y = Corners[[corner]],
                                          cells= T))
    ColRow <- ind2sub(Raster,ex.df$cell)
    BB_XYcoords[[corner]] <- data.frame('row' = ColRow$row,
                                        'col' = ColRow$col)
  }
  return(BB_XYcoords)
}
