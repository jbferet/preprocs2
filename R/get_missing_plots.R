#' return vector grid corresponding to an area of interest
#'
#' @param plots character. list of vector plots
#' @param resume boolean. set TRUE if continue already started process
#' @param datetime list. datetime of acquisition
#' @param output_dir character. output dir
#' @param pattern character. pattern to look for in output_dir
#'
#' @return missing plot ID
#' @importFrom stringr str_split
#' @importFrom lubridate year
#' @export

get_missing_plots <- function(plots, resume, datetime, output_dir,
                              pattern = NULL){

  if (is.null(pattern))
    pattern <- 'plot'
  missing <- seq_len(length(plots))
  if (resume){
    minyear <- lubridate::year(datetime$from)
    s2dl <- stringr::str_split(string = list.files(output_dir, pattern = '.tiff'),
                               pattern = paste0('_', minyear))
    s2dl <- unlist(unique(lapply(X = s2dl, '[[', 1)))
    pattern_ <- paste0(pattern, '_')
    s2dl <-  stringr::str_split(string = s2dl, pattern = pattern_)
    s2dl <- unlist(unique(lapply(X = s2dl, '[[', 2)))
    s2dl <-  unlist(unique(lapply(X = stringr::str_split(string = s2dl,
                                                         pattern = '_'),
                                  '[[', 1)))
    s2dl <- as.numeric(s2dl)
    missing <- which(is.na(match(x = seq_len(length(plots)), s2dl)))
    # s2dl <- as.numeric(unlist(unique(lapply(X = s2dl, '[[', 2))))
    # missing <- which(is.na(match(x = seq_len(length(plots)), s2dl)))
  }
  return(missing)
}
