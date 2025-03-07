#' checks if SI already computed
#'
#' @param dsn_grid list.
#' @param output_SI character. path for output directory for SI
#' @param SI_list character list of SI
#'
#' @return plots list of plots
#' @importFrom sf st_read
#' @importFrom crsuggest suggest_crs
#' @export

check_SI_computed <- function(dsn_grid, output_SI, SI_list){

  message('Reading grid')
  aoi_grid <- sf::st_read(dsn = dsn_grid, quiet = T)
  nbcells <- length(aoi_grid$geom)
  crs_target <- suppressMessages(crsuggest::suggest_top_crs(input = aoi_grid,
                                                            units = 'm'))
  plots <- get_plot_list(dsn = dsn_grid, nbdigits = nchar(nbcells))
  listSI <- ID <- list()
  for (si in SI_list){
    listSI[[si]] <- list.files(path = output_SI, pattern = si)
    ID[[si]] <- gsub(pattern = paste0(siteName, '_'), replacement = '', x = listSI[[si]])
    ID[[si]] <- gsub(pattern = paste0('_', si, '.tiff'), replacement = '', x = ID[[si]])
  }
  ID_all <- unlist(ID)
  already_computed <- names(which(table(ID_all)==length(SI_list)))
  if (!is.null(already_computed)){
    to_compute <- setdiff(x = names(plots), y = already_computed)
    plots <- plots[to_compute]
  }
  tobedone <- list('plots' = plots, 'crs_target' = crs_target)
  return(tobedone)
}
