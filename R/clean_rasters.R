#' clean raster directory from rasters corresponding to plot of interest
#'
#' @param raster_dir character. directory to store collection corrresponding to each plot
#' @param Acqdates character. list fo dates of acquisition
#' @param iChar character. plot ID
#'
#' @return none
#' @export
#'
clean_rasters <- function(raster_dir, Acqdates, iChar){
  for (dayt in Acqdates){
    imgName <- file.path(raster_dir,paste0('plot_',iChar,'_',as.Date(dayt),'_SCL.tiff'))
    unlink(imgName)
    imgName <- file.path(raster_dir,paste0('plot_',iChar,'_',as.Date(dayt),'_CLM.tiff'))
    unlink(imgName)
    imgName <- file.path(raster_dir,paste0('plot_',iChar,'_',as.Date(dayt),'.tiff'))
    unlink(imgName)
  }
  return()
}
