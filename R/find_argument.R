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
  if (any(!is.na(arguments))) {
    if (is.list(arguments)) {
      if (which_argument %in% names(arguments)) {
        if (is_numeric) {
          if (!is.numeric(arguments[[which_argument]])) {
            stop(
              paste0(
                "A ",
                which_argument,
                " argument was supplied, but ",
                which_argument,
                " = '",
                arguments[[which_argument]],
                "' is not numeric."
              )
            )
          }
        }
        argum <- arguments[[which_argument]]
        if (!missing(acceptable)) {
          if (!(argum %in% acceptable)) {
            stop(
              paste0(
                "An argument was supplied, but ",
                which_argument,
                " = '",
                argum,
                "' is not among the accepted choices, which include: ",
                paste(
                  acceptable,
                  sep = ", ",
                  collapse = ", "
                )
              )
            )
          }
        }
      }
    }
  }
  argum
}
