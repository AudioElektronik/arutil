#' strptime in TR locale
#'
#' This function is a mirror for strptime function. The only difference
#' is that it sets the locale to tr_TR before calling it. After completion,
#' it sets the locale back to whatever it was before.
#'
#' @return \link[base]{POSIXlt}
#' @export
strptime_tr <- function(x, format, tz = "") {
  # Making sure that system locale stays the same after this function returns.
  sys_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", sys_lc_time))

  # Changing sys time to TR
  Sys.setlocale("LC_TIME", "tr_TR")

  strptime(x, format, tz)
}
