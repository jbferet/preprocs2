#' get datetime based on a set of parameters
#'
#' @param YYYY character. Year of acquisition
#' @param MMDD_start character. starting Month and Day of acquisition
#' @param duration character. duration in month
#'
#' @return list
#' @importFrom lubridate add_with_rollback
#' @export

datetime_from_to <- function(YYYY, MMDD_start, duration){
  startperiod <- as.Date(paste0(YYYY,MMDD_start))
  endperiod <- lubridate::add_with_rollback(startperiod, months(duration),
                                            roll_to_first = TRUE, preserve_hms = FALSE)-1
  return(list('from' = startperiod, 'to' = endperiod))
}
