#' Point Estimate and CI for Survival Differences
#'
#' @param data Data set
#' @param confounders String of covariates
#' @param risk_percent Display risk differences as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param type Estimand
#' @param risk_digits Digits for risks
#' @param nmin Suppress counts below
#' @param exposure Name of exposure variable
#' @param na_rm Remove observations with missing outcome data
#' @param arguments List of optional arguments
#' @param ci Confidence interval width
#' @param pattern Regex pattern for removing regression terms
#' @param xlevels Strata of exposure variable
#' @param reference Label for reference category
#' @param event Name of event variable
#' @param time Name of time variable
#' @param time2 Name of second time variable, if any
#' @param event_type Level of event variable with competing risks, if any
#' @param ...
#'
#' @return Tibble
#' @noRd
estimate_survdiff <- function(
    data,
    type,
    event,
    time,
    time2,
    exposure,
    confounders,
    digits,
    risk_percent,
    risk_digits,
    is_trend,
    nmin,
    na_rm,
    ci,
    pattern,
    xlevels,
    to,
    reference,
    arguments,
    event_type,
    ...) {
  if(is_trend)
    return(tibble::tibble())
  if(is.na(exposure)) {  # no exposure variable given
    return(
      tibble::tibble(
        .exposure = "Overall",
        res = ""))
  }
  check_event_time(
    data = data,
    type = type,
    event = event,
    time = time,
    time2 = time2)
  digits <- find_rounding_digits(
    digits = digits,
    default = risk_digits)
  timepoint <- find_argument(
    arguments = arguments,
    which_argument = "timepoint",
    is_numeric = TRUE,
    default = NA)
  if(is.na(timepoint))
    stop(
      paste0(
        "Must provide a time horizon for survival analysis of type '",
        type, "'. Example 'design': arguments = list(timepoint = 123)"))

  survdiff_ci(
    formula = stats::as.formula(
      paste0(
        dplyr::if_else(
          is.na(time2),
          true = "survival::Surv(time = .time, ",
          false = "survival::Surv(time = .time_orig, time2 = .time2, "),
        "event = .event_compete) ~ .exposure")),
    data = data,
    time = timepoint,
    estimand = dplyr::if_else(
      stringr::str_detect(
        string = type,
        pattern = "surv"),
      true = "survival",
      false = "cuminc"),
    type = dplyr::if_else(
      stringr::str_detect(
        string = type,
        pattern = "diff"),
      true = "diff",
      false = "ratio"),
    conf.level = ci,
    event_type = event_type,
    id_variable = find_argument(
      arguments = arguments,
      which_argument = "id",
      is_numeric = FALSE,
      default = NULL
    )
  ) %>%
    dplyr::mutate(
      term = paste0(".exposure", .data$term)) %>%
    format_regression_results(
      data = data,
      suppress = "event",
      is_trend = FALSE,
      multiply = dplyr::if_else(
        risk_percent == TRUE,
        true = 100,
        false = 1),
      digits = digits,
      pattern = pattern,
      xlevels = xlevels,
      reference = dplyr::if_else(
        stringr::str_detect(
          string = type,
          pattern = "diff"),
        true = 0,
        false = 1),
      nmin = nmin,
      to = to,
      reference_label = reference,
      percent = risk_percent,
      ratio_digits_decrease = NULL)
}
