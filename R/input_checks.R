#' Check That Outcome Is Binary, Else Abort
#'
#' @param data Data set
#' @param type Estimator
#' @param outcome Name of outcome variable
#'
#' @return Nothing
#'
#' @noRd
check_outcome <- function(
    data,
    type,
    outcome,
    outcome_type = NULL) {
  if(missing(outcome) | !any(names(data) == ".outcome"))
    stop(
      paste0(
        "For type = '",
        type,
        "': The 'design' must contain an 'outcome' variable that exists in ",
        "the 'data'."))
  if(outcome_type == "binary") {
    if(!(all(data$.outcome %in% c(0, 1, NA)) |
         all(data$.outcome %in% c(FALSE, TRUE, NA))))
      stop(
        paste0(
          "type = '",
          type,
          "': Outcome variable '",
          outcome,
          "' must be binary with levels c(0, 1) or c(FALSE, TRUE)."))
  }
  if(outcome_type == "continuous") {
    if(!is.numeric(data$.outcome))
      stop(
        paste0(
          "type = '",
          type,
          "': Outcome variable '",
          outcome,
          "' must be continuous (numeric). Its current class is '",
          class(data$.outcome),
          "'."))
  }
}

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
  if(is.na(digits))
    return(default)
  if(!is.numeric(digits))
    stop(
      paste0(
        "'digits' value for rounding, if provided, must be numeric. '",
        digits,
        "' is not numeric."))
  if(!(digits %in% 0:10))
    stop(
      paste0(
        "'digits' value for rounding, if provided, must be an integer",
        " number from 0 to 10. '",
        digits,
        "' is not."))
  digits
}

#' Find Arguments
#'
#' @param arguments List
#' @param which_argument Element name
#' @param is_numeric Whether element must be numeric
#' @param acceptable List of acceptable values
#' @param default Default if argument does not exist
#'
#' @return Vector
#' @noRd
find_argument <- function(
    arguments,
    which_argument,
    is_numeric,
    acceptable = NULL,
    default = NA) {
  argum <- default
  if(any(!is.na(arguments))) {
    if(is.list(arguments)) {
      if(which_argument %in% names(arguments)) {
        if(is_numeric) {
          if(!is.numeric(arguments[[which_argument]])) {
            stop(
              paste0(
                "A ",
                which_argument,
                " argument was supplied, but ",
                which_argument,
                " = '",
                arguments[[which_argument]],
                "' is not numeric.")
            )
          }
        }
        argum <- arguments[[which_argument]]
        if(!missing(acceptable)) {
          if(!(argum %in% acceptable)) {
            stop(
              paste0(
                "An argument was supplied, but ",
                which_argument,
                " = '",
                argum,
                " is not among the accepted choices, which include: ",
                acceptable))
          }
        }
      }
    }
  }
  argum
}
