#' checks if a string can convert to date
#'
#' @param date_string character.

can_convert_to_date <- function(date_string) {
  tryCatch(
    {
      as.Date(date_string)
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}
