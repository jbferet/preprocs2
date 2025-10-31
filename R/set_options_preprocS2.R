#' set options
#'
#' @param fun character. name of the function which has optional parameters
#' @param options list. including
#' - cloudcover
#' - path_S2tilinggrid
#' - overwrite
#' - geomAcq
#' - additional_process
#' - bands2correct
#' - fraction_vegetation
#' - RadiometricFilter
#'
#' @return options with default values when missing
#' @export

set_options_preprocS2 <- function(fun, options = NULL){

  if (fun == 'get_s2_raster'){
    if (is.null(options$cloudcover))
      options$cloudcover <- 100
    if (is.null(options$path_S2tilinggrid))
      options$path_S2tilinggrid <- NULL
    if (is.null(options$overwrite))
      options$overwrite <- T
    if (is.null(options$geomAcq))
      options$geomAcq <- F
    if (is.null(options$additional_process))
      options$additional_process <- NULL
    if (is.null(options$bands2correct))
      options$bands2correct <- c('B8A', 'B11', 'B12')
    if (is.null(options$fraction_vegetation))
      options$fraction_vegetation <- 5
    if (is.null(options$RadiometricFilter))
      options$RadiometricFilter <- NULL
  }
  return(options)
}

