#' Removing duplicates from a data.frame
#'
#' Used for piping a data.frame variant with possibly duplicate elements.
#' Returned frame does not have duplicate elements. Duplicate means, more than
#' one row having identical values for each column.
#'
#' @examples
#' x <- data.frame(x = c(1,1,2), y = c('a', 'a', 'b')) %>%
#'  remove_duplicate()
#'
#' @export
remove_duplicate <- function(frame) {
  if (!inherits(frame, "data.frame")) {
    stop("Input must inherit data.frame")
  }

  frame[!duplicated(frame), ]
}
