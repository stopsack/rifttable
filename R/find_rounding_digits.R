#' Find Digits to Round At
#'
#' @param digits Proposed number of digits
#' @param default Default value
#'
#' @return Numeric: digits
#' @noRd
find_rounding_digits <- function(
    digits = NA,
    default) {
  if (is.na(digits)) {
    return(default)
  }
  if (!is.numeric(digits)) {
    stop(
      paste0(
        "'digits' value for rounding, if provided, must be numeric. '",
        digits,
        "' is not numeric."
      )
    )
  }
  if (!(digits %in% 0:10)) {
    stop(
      paste0(
        "'digits' value for rounding, if provided, must be an integer",
        " number from 0 to 10. '",
        digits,
        "' is not."
      )
    )
  }
  digits
}
