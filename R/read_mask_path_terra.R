#' extract info from main mask corresponding to aoi
#'
#' @param mask_path path for main mask defined over full region
#'
#' @return spatRaster or spatVector
#' @importFrom terra vect rast
#' @importFrom methods as
#' @export
#'
read_mask_path_terra <- function(mask_path) {
  tryCatch(
    {
      suppressWarnings(terra::rast(mask_path))
    },
    error = function(cond) {
      tryCatch(
        {
          suppressWarnings(terra::vect(mask_path))
        },
        error = function(cond) {
          message('variable "mask_path" provided as input is not a path for a valid raster or vector')
        }
      )
    }
  )
}
