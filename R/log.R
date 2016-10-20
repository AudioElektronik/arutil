#' Print message with the current system time
#'
#' This is a wrapper around \code{\link{message}}. The only difference is
#' it passes the current system time as the first argument
#'
#' @export
message_time <- function(...) {
  time <- strftime(Sys.time(), "%H:%M:%S")
  message(time, " - ",...)
}
