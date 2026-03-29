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
  if (missing(outcome) | !any(names(data) == ".outcome")) {
    stop(
      paste0(
        "For type = '",
        type,
        "': The 'design' must contain an 'outcome' variable that exists in ",
        "the 'data'."
      )
    )
  }
  if (outcome_type == "binary") {
    if (!(all(data$.outcome %in% c(0, 1, NA)) |
      all(data$.outcome %in% c(FALSE, TRUE, NA)))) {
      stop(
        paste0(
          "type = '",
          type,
          "': Outcome variable '",
          outcome,
          "' must be binary with levels c(0, 1) or c(FALSE, TRUE)."
        )
      )
    }
  }
  if (outcome_type == "continuous") {
    if (!is.numeric(data$.outcome)) {
      stop(
        paste0(
          "type = '",
          type,
          "': Outcome variable '",
          outcome,
          "' must be continuous (numeric). Its current class is '",
          class(data$.outcome),
          "'."
        )
      )
    }
  }
}

