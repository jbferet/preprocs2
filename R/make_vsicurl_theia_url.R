#' converts URL TO vsicurl_url for THEIA data
#'
#' @param base_url character.
#'
#' @return vsicurl url
#' @export
#'
make_vsicurl_theia_url <- function(base_url) {
  paste0(
    "/vsicurl/", base_url
  )
}
