#' keeps only wanted tiles from a collection
#'
#' @param collection_plot STAC item collection
#' @param s2_tiles tiles to select
#'
#' @return collection_plot updated STAC item collection
#' @export

eliminate_incompleteTiles <- function(collection_plot, s2_tiles){
  # selects tiles which are identified in s2_tiles
  s2id <- unlist(lapply(collection_plot$features,'[[','id'))
  tileID <- get_tile(s2id)
  selacq <- which(!is.na(match(tileID, s2_tiles)))
  collection_plot$features <- collection_plot$features[selacq]
  return(collection_plot)
}
