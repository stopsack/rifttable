#' Stratified Estimates for Binary Outcomes
#'
#' @param data Data set
#' @param risk_percent Display risk differences as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param type Estimand
#' @param risk_digits Digits for risks
#' @param nmin Suppress counts below
#' @param outcome Name of outcome variable
#' @param na_rm Remove observations with missing outcome data
#' @param ci Confidence interval width
#' @param ... Additional arguments
#'
#' @return Tibble
#' @noRd
estimate_outcome_binary <- function(
    data,
    type,
    outcome,
    digits,
    nmin,
    na_rm,
    ci,
    risk_digits,
    risk_percent,
    to,
    is_trend,
    ...) {
  if(is_trend)
    return(tibble::tibble())
  check_outcome(
    data = data,
    type = type,
    outcome = outcome,
    outcome_type = "binary")
  digits <- find_rounding_digits(
    digits = digits,
    default = risk_digits)
  data <- data %>%
    dplyr::group_by(
      .data$.exposure,
      .drop = FALSE)
  percent_symbol <- dplyr::if_else(
    risk_percent == TRUE,
    true = "%",
    false = "")
  percent_100 <- dplyr::if_else(
    risk_percent == TRUE,
    true = 100,
    false = 1)

  switch(
    EXPR = type,
    "outcomes" = {
      data %>%
        dplyr::summarize(res = paste(sum(.data$.outcome)))
    },
    "outcomes/total" = {
      data %>%
        dplyr::summarize(res = paste(
          sum(.data$.outcome),
          dplyr::n(),
          sep = "/"))
    },
    "cases/controls" = {
      data %>%
        dplyr::summarize(res = paste(
          sum(.data$.outcome),
          sum(!.data$.outcome),
          sep = "/"))
    },
    "risk" = {
      data %>%
        dplyr::summarize(res = paste0(
          format_round(
            sum(.data$.outcome) /
              dplyr::n() *
              percent_100,
            digits = digits),
          percent_symbol))
    },
    "risk (ci)" = {
      data %>%
        dplyr::summarize(res = paste0(
          format_round(
                sum(.data$.outcome) /
                  dplyr::n() *
                  percent_100,
                digits = digits),
          percent_symbol,
          " (",
          format_round(
            scoreci(
              success = sum(.data$.outcome),
              total = dplyr::n(),
              level = ci)$conf.low *
              percent_100,
            digits = digits),
          to,
          format_round(
            scoreci(
              success = sum(.data$.outcome),
              total = dplyr::n(),
              level = ci)$conf.high *
              percent_100,
            digits = digits),
          ")"))
    },
    "outcomes (risk)" = {
      data %>%
        dplyr::summarize(res = paste0(
          sum(.data$.outcome),
          " (",
          format_round(
            sum(.data$.outcome) /
              dplyr::n() *
              percent_100,
            digits = digits),
          percent_symbol,
          ")"))
    },
    "outcomes/total (risk)" = {
      data %>%
        dplyr::summarize(res = paste0(
          sum(.data$.outcome),
          "/",
          dplyr::n(),
          " (",
          format_round(
            sum(.data$.outcome) /
              dplyr::n() *
              percent_100,
            digits = digits),
          percent_symbol,
          ")"))
    }) %>%
    format_stratified_results(
      data = data,
      to = to,
      nmin = nmin,
      suppress = "binary",
      is_trend = is_trend)
}
