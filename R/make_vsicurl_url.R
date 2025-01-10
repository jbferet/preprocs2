#' converts URL TO vsicurl_url
#'
#' @param base_url character.
#' @param collection character.
#'
#' @return vsicurl url
#' @export
#'
make_vsicurl_url <- function(base_url, collection = 'sentinel-2-l2a') {
  paste0(
    "/vsicurl",
    "?pc_url_signing=yes",
    # "?pc_url_signing=no",
    # "&pc_collection=sentinel-2-l2a",
    "&pc_collection=", collection,
    "&url=", base_url
  )
}
