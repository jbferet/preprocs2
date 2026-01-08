#' get S2 minimum and maximum angles for geometry of acquisition
#'
#' @param path_angles character path for raster angles of acquisition dowloaded with preprocS2
#' @param path_bbox character path for GPKG defining bounding box of study site
#' @param dateAcq date. date of aquisition
#'
#' @return list
#' @importFrom terra rast crop values project
#' @export

get_s2_angles <- function(path_angles, path_bbox, dateAcq = NULL){

  # check path for angle rasters
  angles <- c('saa', 'sza', 'vaa', 'vza')
  if (inherits(x = path_angles, what = 'list')){
    if (FALSE %in% file.exists(unlist(path_angles))){
      message('please provide either')
      message('- path for geometry of acquisition saved by preprocS2 in "geom_acq_S2" directory')
      message('or' )
      message('- list of files "saa", "sza", "vaa" and "vza" including aoi')
      stop_quietly()
    }
    if (FALSE %in% angles %in%names(path_angles)){
      message('The list of files defined in "path_angles" should include elements named "saa", "sza", "vaa" and "vza" and including aoi')
      stop_quietly()
    }
    angles_path <- path_angles
  } else if (dir.exists(path_angles) & !is.null(dateAcq)){
    # define path for angle rasters
    path_angles <- as.list(file.path(path_angles,
                                     paste(angles, '_', dateAcq, '.tiff', sep = '')))
    if (FALSE %in% file.exists(unlist(path_angles))){
      message('These files are needed but were not found:')
      print(path_angles)
      message('please provide either')
      message('- path for geometry of acquisition saved by preprocS2 in "geom_acq_S2" directory')
      message('or' )
      message('- list of files "saa", "sza", "vaa" and "vza" including aoi')
      stop_quietly()
    }
    angles_path <- path_angles
    names(angles_path) <- angles
  }
  # define site location based on bbox
  bbox <- terra::vect(x = path_bbox)
  geom <- MinAngle <- MaxAngle <- list()
  if (!terra::crs(x = terra::rast(angles_path$saa)) ==  terra::crs(x = bbox))
    bbox <- terra::project(x = bbox, y = terra::rast(angles_path$saa))
  # get min and max values for each angle
  for (angle in angles){
    geom[[angle]] <- terra::crop(x = terra::rast(angles_path[[angle]]), y = bbox)
    MinAngle[[angle]] <- min(terra::values(geom[[angle]]))
    MaxAngle[[angle]] <- max(terra::values(geom[[angle]]))
  }
  psi <- terra::values(geom$saa)-terra::values(geom$vaa)
  MinAngle$psi <- min(abs(psi))
  MaxAngle$psi <- max(abs(psi))
  if (MinAngle$psi>180)
    MinAngle$psi <- 360-MinAngle$psi
  if (MaxAngle$psi>180)
    MaxAngle$psi <- 360-MaxAngle$psi
  MinAngle <- unlist(MinAngle)
  MaxAngle <- unlist(MaxAngle)
  return(list('MinAngle' = MinAngle, 'MaxAngle' = MaxAngle))
}
