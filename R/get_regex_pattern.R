#' Extract regex pattern from character vectors
#'
#' Given a set of character vector, extracts the given regex pattern out
#' of every single element of that vector.
#'
#' @param char_vector Character vector that the regex will be extracted from
#' @param pattern Regex pattern to extract
#'
#' @examples
#' # Getting digits out of name-digit combinations
#' x <- c("John-1", "Jess-2", "Peter-4")
#' get_regex_pattern(x, "[A-Za-z]+-([0-9]+)")
#'
#' @return Extracted regex patterns as a character vector
#' @export
get_regex_pattern <- function(char_vector, pattern) {
  # Checking input character vector
  if (!is.character(char_vector)) {
    stop("Input is not a character vector")
  }

  # Checking pattern
  valid_pattern_type <- (
    is.character(pattern)
    & length(pattern) == 1
    & nchar(pattern) != 0)

  if (!valid_pattern_type) {
    stop("Pattern must be a non empty chracter vector of length 1")
  }

  gsubfn::strapply(char_vector, pattern, empty = "", simplify = T)
}
