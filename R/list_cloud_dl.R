#' checks if cloud data already downloaded
#'
#' @param raster_dir character. directory to store collection corrresponding to each plot
#' @param days character. list fo dates of acquisition
#' @param iChar character. plot ID
#' @param asset_names character.
#'
#' @return list of collections per plot
#' @export
#'
list_cloud_dl <- function(raster_dir, days, iChar, asset_names){
  # check if already downloaded
  Cloud_ok <- list()
  days_ok <- days_dl <- c()
  i <- 0
  for (d in days){
    i <- i +1
    imgName <- file.path(raster_dir,paste0('plot_',iChar,'_',as.Date(d),'_',asset_names,'.tiff'))
    if (file.exists(imgName)){
      days_ok <- c(days_ok, d)
      Cloud_ok[[i]] <- imgName
    } else {
      days_dl <- c(days_dl, d)
      Cloud_ok[[i]] <- NULL
    }
  }
  return(list('Cloud_ok' = Cloud_ok,
              'days_dl' = as.Date(days_dl),
              'days_ok' = as.Date(days_ok)))
}
