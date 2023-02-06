#' Trim, format, and round a number
#'
#' @param x Number
#' @param digits Digits to round to
#'
#' @return Character
#'
#' @noRd
format_round <- function(x, digits) {
  format(
    round(
      x,
      digits = digits),
    nsmall = digits,
    trim = TRUE,
    scientific = FALSE)
}
