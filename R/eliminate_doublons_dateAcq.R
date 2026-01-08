#' eliminates doublons in terms of date of acquisition from collection
#'
#' @param item_collection collection STAC item collection
#' @param aoistac spatVector
#' @param stac_info list
#'
#' @return collection STAC item collection
#' @export

eliminate_doublons_dateAcq <- function(item_collection, aoistac = NULL,
                                       stac_info = NULL){
  s2id <- unlist(lapply(item_collection$features,'[[','id'))
  tileID <- get_tile(s2id)
  if (length(unique(tileID))>1){
    tileDist <- table(tileID)
    tileSelect <- names(tileDist[which(tileDist== max(tileDist))[1]])
    sel <- which(tileID==tileSelect)
    item_collection$features <- item_collection$features[sel]
  }
  # case when several acquisitions over same region
  # --> crossing orbits for S2A and S2B or S2C for the same acquisition date
  dateacq <- get_dateAcq(item_collection = item_collection, stac_info = stac_info)
  dist_dateacq <- table(dateacq)
  if (max(dist_dateacq)>1){
    doublons <- which(dist_dateacq>1)
    for (doublon in names(doublons)){
      selacqs <- which(dateacq==doublon)
      tileID_acqs <- tileID[selacqs]
      if (length(unique(tileID_acqs))==1 & length(tileID_acqs)>1 & !is.null(aoistac)){
        asset_names <- get_cloud_asset(stac_info = stac_info)
        item_collection_acq <- item_collection
        item_collection_acq$features <- item_collection_acq$features[selacqs]
        features_dl <- get_asset_terra(item = item_collection_acq,
                                       asset_names = asset_names,
                                       collection = stac_info$collection,
                                       aoi = aoistac)
        # identify acquisition with most valid pixels
        nbpix <- lapply(X = features_dl, FUN = function(x) length(na.omit(terra::values(x))))
        elimfeature <- which(unlist(nbpix)==min(unlist(nbpix)))[1]
        item_collection$features <- item_collection$features[-selacqs[elimfeature]]
        dateacq <- dateacq[-selacqs[elimfeature]]
        tileID <- tileID[-selacqs[elimfeature]]
        # selfeature <- which(unlist(nbpix)==max(unlist(nbpix)))[1]
        # item_collection$features <- item_collection$features[selfeature]
      }
    }
  }
  # if (length(unique(tileID))==1 & length(tileID)>1 & !is.null(aoistac) &
  #     length(dateacq)==1){
  #   asset_names <- get_cloud_asset(stac_info = stac_info)
  #   features_dl <- get_asset_terra(item = item_collection,
  #                                  asset_names = asset_names,
  #                                  collection = stac_info$collection,
  #                                  aoi = aoistac)
  #   # identify acquisition with most valid pixels
  #   nbpix <- lapply(X = features_dl, FUN = function(x) length(na.omit(terra::values(x))))
  #   selfeature <- which(unlist(nbpix)==max(unlist(nbpix)))[1]
  #   item_collection$features <- item_collection$features[selfeature]
  # }
  return(item_collection)
}

