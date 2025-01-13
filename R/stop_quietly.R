#' stops quietly without error message
#'
#' @export
#'
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}