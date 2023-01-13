#' Find exposure categories with sparse data:
#' observations or events below "nmin"
#'
#' @param data  Dataset
#' @param event Event variable
#' @param estimand  Estimand
#' @param is_trend Whether trend is estimated
#'
#' @return Tibble
#' @noRd
counts_per_stratum <- function(data, event, estimand, is_trend) {
  if(is_trend == TRUE) {
    data <- data %>%
      dplyr::mutate(.exposure = "")
  }

  if(stringr::str_detect(
    string = estimand,
    pattern = "rmtdiff|rmtl|^surv|cuminc|^hr|^irr|events|^rate|medsurv|medfu|maxfu") &
    !stringr::str_detect(
      string = estimand,
      pattern = "irrrob")) {
    data <- data %>%
      dplyr::select(".exposure",
                    .event = {{ event }}) %>%
      dplyr::filter(as.logical(.data$.event))  # == TRUE
  }
  data %>%
    dplyr::count(
      .data$.exposure,
      name = ".per_stratum",
      .drop = FALSE)
}
