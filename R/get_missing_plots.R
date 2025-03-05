#' return vector grid corresponding to an area of interest
#'
#' @param plots character. path for vector file defining aoi
#' @param pursue_existing numeric. square cell size in meters
#' @param datetime character. output path
#' @param output_dir character. path corresponding to output dir
#'
#' @return missing plot ID
#' @importFrom stringr str_split
#' @importFrom lubridate year
#' @export

get_missing_plots <- function(plots, pursue_existing, datetime, output_dir){

  missing <- seq_len(length(plots))
  if (pursue_existing){
    minyear <- lubridate::year(datetime$from)
    s2dl <- stringr::str_split(string = list.files(output_dir),
                               pattern = paste0('_', minyear))
    s2dl <- unlist(unique(lapply(X = s2dl, '[[', 1)))
    s2dl <-  stringr::str_split(string = s2dl, pattern = 'plot_')
    s2dl <- as.numeric(unlist(unique(lapply(X = s2dl, '[[', 2))))
    missing <- which(is.na(match(x = seq_len(length(plots)), s2dl)))
  }
  return(missing)
}
