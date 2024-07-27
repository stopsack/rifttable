#' Find exposure categories with sparse data:
#' observations or events below "nmin"
#'
#' @param data  Dataset
#' @param is_trend Whether trend is estimated
#' @param suppress Suppress if below total count, no outcomes, or no events
#'
#' @return Tibble
#' @noRd
counts_per_stratum <- function(
    data,
    suppress = c("total", "binary", "event"),
    is_trend) {
  # Counts per stratum for nmin
  suppress <- match.arg(suppress)
  if (is_trend == TRUE) {
    data <- data %>%
      dplyr::mutate(.exposure = "")
  }

  if (suppress == "binary") {
    data <- data %>%
      dplyr::filter(as.logical(.data$.outcome))
  }

  if (suppress == "event") {
    data <- data %>%
      dplyr::filter(as.logical(.data$.event))
  }

  data %>%
    dplyr::count(
      .data$.exposure,
      name = ".per_stratum",
      .drop = FALSE
    )
}
