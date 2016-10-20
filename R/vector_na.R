#' Stripping a vector from NA's
#'
#' @param vector a vector that may have NA's.
#'
#' @export
strip_na <- function(vector) {
  vector[!is.na(vector)]
}

#' Changing occurances of NULL to NA
#'
#' This is usefull if you want to simplify a list into a
#' built in vector and use it in a data_frame.
#'
#' @export
null_to_na <- function(vector) {
  vector[sapply(vector, is.null)] <- NA
  return(vector)
}
