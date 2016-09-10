#' Changing turkish characters to ascii counterparts
#'
#' @examples
#' tr_char_to_ascii(c("Şekil", "Kağnı", "Otobüs"))
#'
#' @return character vector without Turkish characters
#' @export
tr_char_to_ascii <- function(char_vector) {
  if (!is.character(char_vector)) {
    stop("Input is not a character vector.")
  }

  trans_frame <- tibble::frame_data(
    ~tr, ~ascii,
    "ş", "s",
    "Ş", "S",
    "ç", "c",
    "Ç", "C",
    "ğ", "g",
    "Ğ", "G",
    "ö", "o",
    "Ö", "O",
    "ü", "u",
    "Ü", "U",
    "ı", "i",
    "İ", "I")

  Reduce(function(char, trans) {
    gsub(trans[1], trans[2], char)
  },

  # Transposing to be able to loop over char couples
  tibble::as_data_frame(t(trans_frame)),

  char_vector)
}
