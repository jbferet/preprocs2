#' downloads S2 reflectance to update the vegetation mask
#'
#' @param item_collection path for collection to download
#' @param band_url list.
#' @param rootname character. directory where rasters are stored
#'
#' @return list of collections per plot
#' @importFrom stringr str_detect
#' @export
#'
check_existing_mask <- function(item_collection, band_url, rootname){
  asset_names <- c('B02', 'B04', 'B08')
  # get bands per acquisition
  selAcq <- list()
  collec_dl <- item_collection
  # prepare if part already processed
  # is mask already updated ? intitalize to FALSE
  df_acq <- data.frame('acq' = item_collection$acquisitionDate, 'already' = F)
  for (dateacq in item_collection$acquisitionDate){
    dateacq1 <- as.character(as.Date(dateacq))
    dateacq2 <- format(as.Date(dateacq),format = '%Y%m%d')
    sel <- which(stringr::str_detect(string = band_url,
                                     pattern = paste0('MSIL2A_',dateacq2)))
    selAcq[[dateacq1]] <- check_order_bands(acq = band_url[sel],
                                            patterns = asset_names)
    # check if mask exists and eliminate from collection to download
    out_udmask_file <- paste0(rootname, as.Date(dateacq), '_BIN_v2.tiff')
    if (file.exists(out_udmask_file)) {
      selAcq[[dateacq1]] <- NULL
      elim <- which(collec_dl$acquisitionDate == dateacq1)
      collec_dl$acquisitionDate <- collec_dl$acquisitionDate[-elim]
      collec_dl$features <- collec_dl$features[-elim]
      elim2 <- which(item_collection$acquisitionDate == dateacq1)
      df_acq$already[elim2] <- T
    }
  }
  return(list('collec_dl' = collec_dl, 'df_acq' = df_acq, 'selAcq' = selAcq))
}
