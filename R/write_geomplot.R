#' extracts geometry of acquisition for each plot
#'
#' @param DatesofAcq date. date of acquisition
#' @param geom_dir character. path for geometry of acquisition main dir
#' @param geom_subdir character. path for geometry of acquisition subdir
#' @param list_aoi list.
#' @param p list.
#'
#' @return list of collections per plot
#' @importFrom terra rast writeRaster
#' @importFrom methods as
#' @export
#'
write_geomplot <- function(DatesofAcq, geom_dir, geom_subdir, list_aoi, p = NULL){
  angle <- c('saa', 'sza', 'vaa', 'vza')
  fileName <- file.path(geom_dir, paste0(angle, '_', as.character(DatesofAcq), '.tiff'))
  for (j in 1:4){
    for (aoiID in names(list_aoi)){
      fileangle <- file.path(geom_subdir,
                             paste0('plot_',aoiID,'_',angle[j],
                                    '_', as.character(DatesofAcq), '.tiff'))
      if (!file.exists(fileangle)){
        geom <- terra::crop(x = terra::rast(fileName[j]),
                            y = as(list_aoi[[aoiID]], "Spatial"))
        terra::writeRaster(x = geom, filename = fileangle)
      }
    }
  }
  if (!is.null(p))
    p()
}
