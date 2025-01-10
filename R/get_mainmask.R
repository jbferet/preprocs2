#' extract info from main mask corresponding to aoi
#'
#' @param mask_path path for main mask defined over full region
#' @param S2_dl plot ID
#' @param aoiplot geometry corresponding to aoi
#'
#' @return list of collections per plot
#' @importFrom terra crop rast project crs
#' @importFrom methods as
#' @export
#'
get_mainmask <- function(mask_path, S2_dl, aoiplot){
  if (!is.null(mask_path)){
    if (file.exists(mask_path)){
      # check if same projection
      if (crs(terra::rast(mask_path)) == crs(S2_dl)){
        mainmask <- terra::crop(x = terra::rast(mask_path), y = as(aoiplot, "Spatial"))
        mainmask <- terra::project(x = mainmask, y = S2_dl)
      } else if (!crs(terra::rast(mask_path)) == crs(S2_dl)){
        mainmask <- terra::project(x = terra::rast(mask_path), y = S2_dl)
      }
      mainmask[mainmask>0] <- 1
      names(mainmask) <- 'binary mask'
    } else {
      mainmask <- 1+0*S2_dl[[3]]
      names(mainmask) <- 'binary mask'
    }
  } else {
    mainmask <- 1+0*S2_dl[[3]]
    names(mainmask) <- 'binary mask'
  }
  return(mainmask)
}
