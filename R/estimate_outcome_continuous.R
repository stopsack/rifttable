#' Stratified Estimates for Continuous Outcomes
#'
#' @param data Data set
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param type Estimand
#' @param outcome Outcome variable
#' @param nmin Suppress counts below
#' @param na_rm Remove observations with missing outcome data
#' @param diff_digits Digits for differences
#' @param ci Confidence interval width
#' @param ... Additional arguments
#'
#' @return Tibble
#' @noRd
estimate_outcome_continuous <- function(
    data,
    type,
    outcome,
    digits,
    nmin,
    na_rm,
    ci,
    diff_digits,
    to,
    is_trend,
    ...) {
  if(is_trend)
    return(tibble::tibble())
  if(type != "total")
    check_outcome(
      data = data,
      type = type,
      outcome = outcome,
      outcome_type = "continuous")
  digits <- find_rounding_digits(
    digits = digits,
    default = diff_digits)
  data <- data %>%
    dplyr::group_by(
      .data$.exposure,
      .drop = FALSE)

  switch(
    EXPR = type,
    "total" = {
      data %>%
        dplyr::summarize(res = paste(dplyr::n()))
    },
    "mean" = {
      data %>%
        dplyr::summarize(
          res = format_round(
            mean(.data$.outcome),
            digits = digits))
    },
    "mean (sd)" = {
      data %>%
        dplyr::summarize(
          res = paste0(
            format_round(
              mean(.data$.outcome),
              digits = digits),
            " (",
            format_round(
              stats::sd(.data$.outcome),
              digits = digits),
            ")"))
    },
    "sd" = {
      data %>%
        dplyr::summarize(res = format_round(
          stats::sd(.data$.outcome),
          digits = digits))
    },
    "mean (ci)" = {
      data %>%
        dplyr::summarize(res = paste0(
          format_round(
            mean(.data$.outcome),
            digits = digits),
          " (",
          format_round(
            mean(.data$.outcome) -
              stats::qnorm(1 - (1 - ci) / 2) *
              sqrt(stats::var(.data$.outcome) /
                     sum(!is.na(.data$.outcome))),
            digits = digits),
          to,
          format_round(
            mean(.data$.outcome) +
              stats::qnorm(1 - (1 - ci) / 2) *
              sqrt(stats::var(.data$.outcome) /
                     sum(!is.na(.data$.outcome))),
            digits = digits),
          ")"))
    },
    "geomean" = {
      data %>%
        dplyr::summarize(
          res = format_round(
            exp(mean(log(.data$.outcome))),
            digits = digits))
    },
    "median" = {
      data %>%
        dplyr::summarize(res = format_round(
          stats::median(.data$.outcome),
          digits = digits))
    },
    "median (iqr)" = {
      data %>%
        dplyr::summarize(res = paste0(
          format_round(
            stats::median(.data$.outcome),
            digits = digits),
          " (",
          format_round(
            stats::quantile(
              .data$.outcome,
              probs = 0.25),
            digits = digits),
          to,
          format_round(
            stats::quantile(
              .data$.outcome,
              probs = 0.75),
            digits = digits),
          ")"))
    },
    "range"  = {
      if(any(!is.na(data$.outcome))) {
        data %>%
          dplyr::summarize(res = paste0(
            format_round(
              min(.data$.outcome),
              digits = digits),
            to,
            format_round(
              max(.data$.outcome),
              digits = digits)))
      } else
        "--"
    },
    stop(paste0("Invalid estimator type = '", type, "'."))
  ) %>%
    format_stratified_results(
      data = data,
      to = to,
      nmin = nmin,
      suppress = "total",
      is_trend = is_trend)
}
