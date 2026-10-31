#' Check That Event and Time Variables are Valid
#'
#' @description The function does not check that the \code{event} variable is
#' binary to allow for variables denoting more than one competing event.
#'
#' @param data Data set
#' @param type Estimator
#' @param event Name of event variable
#' @param time Name of time variable
#' @param time2 Name of optional second (exit) time variable
#'
#' @return Nothing
#'
#' @noRd
check_event_time <- function(
    data,
    type,
    event,
    time,
    time2 = NA) {
  if (missing(event) | !any(names(data) == ".event") |
      missing(time) | !any(names(data) == ".time")
  ) {
    stop(
      paste0(
        "For type = '",
        type,
        "': The 'design' must contain 'event' and 'time' variables that exist ",
        "in the 'data'."
      )
    )
  }
}
