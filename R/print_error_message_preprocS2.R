#' prints an error message if problems occur
#'
#' @param def_error character. nature of the error
#' @param optarg character. optional additional info required for the error message
#'
#' @return none.
#' @export

print_error_message_preprocS2 <- function(def_error, optarg) {
  if (def_error=='missing_hdr'){
    ImPathHDR <- optarg
    message("WARNING : COULD NOT FIND HDR FILE ", ImPathHDR)
  }
  return(invisible())
}
