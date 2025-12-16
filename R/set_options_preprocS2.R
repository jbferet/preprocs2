#' set options
#'
#' @param fun character. name of the function which has optional parameters
#' @param options list. including
#' - cloudcover
#' - path_S2_tiling_grid
#' - overwrite
#' - geom_acq
#' - additional_process
#' - bands_to_correct
#' - fraction_vegetation
#' - radiometric_filter
#'
#' @return options with default values when missing
#' @export

set_options_preprocS2 <- function(fun, options = NULL){

  if (fun == 'get_s2_raster'){
    if (is.null(options$cloudcover))
      options$cloudcover <- 100
    if (is.null(options$path_S2_tiling_grid))
      options$path_S2_tiling_grid <- NULL
    if (is.null(options$overwrite))
      options$overwrite <- T
    if (is.null(options$overwrite_geom_acq))
      options$overwrite_geom_acq <- T
    if (is.null(options$geom_acq))
      options$geom_acq <- F
    if (is.null(options$additional_process))
      options$additional_process <- NULL
    if (is.null(options$bands_to_correct))
      options$bands_to_correct <- c('B8A', 'B11', 'B12')
    if (is.null(options$fraction_vegetation))
      options$fraction_vegetation <- 5
    if (is.null(options$resampling))
      options$resampling <- 'near'
    if (is.null(options$radiometric_filter))
      options$radiometric_filter <- NULL
    if (is.null(options$original_clouds))
      options$original_clouds <- TRUE
  }
  return(options)
}

