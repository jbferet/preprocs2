#' extract info from main mask corresponding to aoi
#'
#' @param mask_path path for main mask defined over full region
#' @param S2_dl plot ID
#' @param aoiplot geometry corresponding to aoi
#'
#' @return list of collections per plot
#' @importFrom terra crop project crs rasterize
#' @importFrom methods as
#' @export
#'
get_mainmask <- function(mask_path, S2_dl, aoiplot){
  if (!is.null(mask_path)){
    if (file.exists(mask_path)){
      mask <- read_mask_path_terra(mask_path)
      if (inherits(S2_dl, 'list'))
        S2_dl <- terra::rast(S2_dl)
      # check if same projection
      if (crs(mask) == crs(S2_dl)){
        mainmask <- terra::crop(x = mask, y = as(aoiplot, "Spatial"))
        mainmask <- terra::project(x = mainmask, y = S2_dl)
      } else if (!crs(mask) == crs(S2_dl)){
        mainmask <- terra::project(x = mask, y = S2_dl)
        mainmask <- terra::crop(x = mainmask, y = as(aoiplot, "Spatial"))
      }
      if (inherits(x = mainmask, what = 'SpatVector')){
        mainmask <- terra::rasterize(x = mainmask, y = S2_dl, background=0)
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
