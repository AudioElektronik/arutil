#' Fill merged cell with previous value
#'
#' Sometimes in an excel sheet or a csv, some cells of a column are merged.
#' Reader can understand what is meant on a proper viewer but when the data
#' is read into R, only one of the merged cell has data and others are \code{NA}
#'
#' This function keeps the record of last cell with a value and if the next cell
#' is \code{NA}, puts the last seen value in previous cells.
#'
#' \strong{CAUTION!} If the cell is actually supposed to be \code{NA}, it still
#' fills is with the last seen value.
#'
#' @param vec A vector that is known to have merged cells
#' @return \code{vec} with \code{NA}'s filled with previous cell value
#'
#' @examples
#' x <- c("A", NA, NA, "B", NA)
#' fill_merged_cell(x)
#'
#' @export
fill_merged_cells <- function(vec) {
  last_seen_value <- NULL

  sapply(vec, USE.NAMES = FALSE, function(elem) {
    # This means the loop just started
    if (is.null(last_seen_value)) {
      last_seen_value <<- elem
      elem # Returning the first value
    } else {
      if (is.na(elem)) {
        last_seen_value
      } else {
        last_seen_value <<- elem
        elem
      }
    }
  })
}
