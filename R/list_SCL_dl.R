#' checks if SCL data already downloaded
#'
#' @param raster_dir character. directory to store collection corrresponding to each plot
#' @param days character. list fo dates of acquisition
#' @param iChar character. plot ID
#'
#' @return list of collections per plot
#' @export
#'
list_SCL_dl <- function(raster_dir, days, iChar){
  # check if already downloaded
  SCL_ok <- SCL_dl <- list()
  days_ok <- days_dl <- c()
  i <- 0
  for (d in days){
    i <- i +1
    imgName <- file.path(raster_dir,paste0('plot_',iChar,'_',as.Date(d),'_SCL.tiff'))
    if (file.exists(imgName)){
      days_ok <- c(days_ok, d)
      SCL_ok[[i]] <- imgName
    } else {
      days_dl <- c(days_dl, d)
      SCL_ok[[i]] <- NULL
    }
  }
  return(list('SCL_ok' = SCL_ok,
              'days_dl' = as.Date(days_dl),
              'days_ok' = as.Date(days_ok)))
}
