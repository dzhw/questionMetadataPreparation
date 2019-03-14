#' Trim multiple leading and trailing whitespace
#'
#' @param x the object whose whitespace should be trimmed

trim_multiple_leading_trailing_ws <- function(x) {
  stringr::str_replace(
    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl = TRUE),
    "^$", NA_character_
  )
}
