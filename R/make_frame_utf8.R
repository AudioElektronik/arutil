#' Changing the encoding of frame columns to utf8
#'
#' Encoding differences can make joins not work properly. This function
#' get a variance of data.frame as input and converts the encoding of every
#' character column in that frame to utf8.
#'
#' @param frame an object inheriting data.frame
#' @return inputted frame with character vectors encoded as utf8
#'
#' @examples
#' # Creating strings encoded latin1
#' x <- c("fa\xe7ile", "\xe7erez")
#' Encoding(x) <- "latin1"
#'
#' # Putting them in a data.frame
#' df <- data.frame(x = x, y = c(1, 2))
#' make_frame_utf8(df)
#'
#' @export
make_frame_utf8 <- function(frame) {
  if (!inherits(frame, "data.frame")) {
    stop("Input must inherit data.frame class.")
  }

  # Encoding is modified only is column is a character vector
  dplyr::mutate_all(
    frame,
    dplyr::funs(
      if (is.character(.))
        enc2utf8(.)
      else
        .
      )
  )
}
