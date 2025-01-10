#' eliminates doublons in terms of date of acquisition from collection
#'
#' @param item_collection collection STAC item collection
#'
#' @return collection STAC item collection
#' @export

eliminate_doublons_dateAcq <- function(item_collection){
  s2id <- unlist(lapply(item_collection$features,'[[','id'))
  tileID <- get_tile(s2id)
  if (length(unique(tileID))>1){
    tileDist <- table(tileID)
    tileSelect <- names(tileDist[which(tileDist== max(tileDist))[1]])
    sel <- which(tileID==tileSelect)
    item_collection$features <- item_collection$features[sel]
  }
  return(item_collection)
}

