#' Point Estimate and CI From Cox Regression Models
#'
#' @param data Data set
#' @param confounders String of covariates
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param type Estimand
#' @param ratio_digits Digits for ratios
#' @param nmin Suppress counts below
#' @param exposure Name of exposure variable
#' @param na_rm Remove observations with missing outcome data
#' @param arguments List of optional arguments
#' @param ci Confidence interval width
#' @param pattern Regex pattern for removing regression terms
#' @param xlevels Strata of exposure variable
#' @param reference Label for reference category
#' @param time2 Name of second time variable, if any
#' @param ... Additional arguments
#'
#' @return Tibble
#' @noRd
estimate_regress_cox <- function(
    data,
    type,
    time2,
    exposure,
    confounders,
    digits,
    ratio_digits,
    is_trend,
    nmin,
    na_rm,
    ci,
    pattern,
    xlevels,
    to,
    reference,
    arguments,
    ...) {
  if(is.na(exposure)) {  # no exposure variable given
    if(is_trend)
      return(tibble::tibble())
    else
      return(
        tibble::tibble(
          .exposure = "Overall",
          res = ""))
  }
  digits <- find_rounding_digits(
    digits = digits,
    default = ratio_digits)

  survival::coxph(
    formula = stats::as.formula(
      paste0(
        dplyr::if_else(
          is.na(time2),
          true = "survival::Surv(time = .time, ",
          false = "survival::Surv(time = .time_orig, time2 = .time2, "),
        "event = .event) ~ .exposure ",
        confounders)),
    data = data) %>%
    broom::tidy(
      conf.int = TRUE,
      conf.level = ci,
      exponentiate = TRUE) %>%
    format_regression_results(
      data = data,
      suppress = "event",
      is_trend = is_trend,
      multiply = 1,
      digits = digits,
      pattern = pattern,
      xlevels = xlevels,
      reference = 1,
      nmin = nmin,
      to = to,
      reference_label = reference)
}
