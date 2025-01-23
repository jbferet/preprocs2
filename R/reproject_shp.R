#' This function reprojects a shapefile and saves reprojected shapefile
#'
#' @param path_vector_init character. path for a shapefile to be reprojected
#' @param SpatialObj object. spatRaster or spatVector
#' @param path_vector_reproj character. path for the reprojected shapefile
#'
#' @return path_vector character. path of the shapefile
#' - path_vector_init if the vector did not need reprojection
#' - path_vector_reproj if the vector needed reprojection
#'
#' @importFrom terra same.crs vect project writeVector
#' @export
#'
reproject_shp = function(path_vector_init,SpatialObj,path_vector_reproj){

  path_vector <- path_vector_init
  vector_init <- terra::vect(x = path_vector_init)
  if (!terra::same.crs(x = vector_init, y = SpatialObj)){
    vector_reproj <- terra::project(x = vector_init, y = SpatialObj)
    terra::writeVector(x = vector_reproj,
                       filename = path_vector_reproj,
                       overwrite = TRUE)
    path_vector <- path_vector_reproj
  }
  return(path_vector)
}
