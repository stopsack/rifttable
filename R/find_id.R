#' Find ID variable for survfit
#'
#' @param data Data set
#' @param id_variable Name of \code{id} variable specified in the \code{design}.
#'
#' @return Vector of IDs. Unique per row if no ID variable is given.
#' @noRd
find_id <- function(data, id_variable) {
  if (!is.null(id_variable)) {
    if (!id_variable %in% names(data)) {
      stop(
        paste0(
          "arguments = list(id = '",
          id_variable,
          "') is not an ID variable that ",
          "is valid for this data set."
        )
      )
    }
    return(
      data |>
        dplyr::pull(
          dplyr::one_of(id_variable)
        )
    )
  } else {
    return(1:nrow(data))
  }
}
