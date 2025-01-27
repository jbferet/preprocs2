#' checks if assets from item list already exist
#'
#' @param S2_items S2 items
#' @param asset_names character.
#' @param dateAcqs dates.
#'
#' @return list of collections per plot
#' @export
#'
list_assets <- function(S2_items, asset_names, dateAcqs){
  asset_names_list <- list()
  for (i in seq_len(length(dateAcqs))){
    dateAcq <- as.character(dateAcqs[i])
    if (!is.null(S2_items[[as.character(dateAcq)]])){
      alreadyExists <- match(names(S2_items[[dateAcq]]), asset_names)
      if (length(alreadyExists)>0){
        asset_names_list[[dateAcq]] <- asset_names[-alreadyExists]
      } else {
        asset_names_list[[dateAcq]] <- asset_names
      }
    } else {
      asset_names_list[[dateAcq]] <- asset_names
    }
  }
  return(asset_names_list)

}
