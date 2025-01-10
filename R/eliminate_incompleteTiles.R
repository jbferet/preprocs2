#' keeps only wanted tiles from a collection
#'
#' @param collection_plot STAC item collection
#' @param S2tiles tiles to select
#'
#' @return collection_plot updated STAC item collection
#' @export

eliminate_incompleteTiles <- function(collection_plot, S2tiles){
  # selects tiles which are identified in S2tiles
  s2id <- unlist(lapply(collection_plot$features,'[[','id'))
  tileID <- get_tile(s2id)
  selacq <- which(!is.na(match(tileID, S2tiles)))
  collection_plot$features <- collection_plot$features[selacq]
  return(collection_plot)
}
