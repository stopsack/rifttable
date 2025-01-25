#' Point Estimate and CI From Cox Regression Models
#'
#' @param data Data set
#' @param confounders String of covariates
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param type Estimand
#' @param ratio_digits Digits for ratios
#' @param ratio_digits_decrease Fewer digits for elevated ratios
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
#' @param ... Additional arguments
#'
#' @return Tibble
#' @noRd
estimate_regress_cox <- function(
    data,
    type,
    event,
    time,
    time2,
    exposure,
    confounders,
    digits,
    ratio_digits,
    ratio_digits_decrease,
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
  if (is.na(exposure)) { # no exposure variable given
    if (is_trend) {
      return(tibble::tibble())
    } else {
      return(
        tibble::tibble(
          .exposure = "Overall",
          res = ""
        )
      )
    }
  }
  if (length(unique(stats::na.omit(data$.exposure))) < 2) { # no contrasts estimable
    return(tibble::tibble(
      .exposure = unique(data$.exposure)[1],
      res = ""
    ))
  }
  check_event_time(
    data = data,
    type = type,
    event = event,
    time = time,
    time2 = time2
  )
  digits <- find_rounding_digits(
    digits = digits,
    default = ratio_digits
  )
  coxph_weights <- find_argument(
    arguments = arguments,
    which_argument = "weights",
    is_numeric = FALSE,
    default = NULL
  )
  if (!is.null(coxph_weights)) {
    stop(paste(
      "Breaking change in rifttable 0.6.3: 'weights' for Cox models must now",
      "be provided as part of the 'design', as for other estimators.",
      "'weights' in the 'arguments' list are no longer supported."
    ))
  }
  # as in survfit.formula: if weights are not all integer, use robust variance
  if(any(data$.weights %% 1 != 0, na.rm = TRUE)) {
    coxph_robust <- TRUE
  }
  coxph_robust <- find_argument(
    arguments = arguments,
    which_argument = "robust",
    is_numeric = FALSE,
    default = NULL
  )

  survival::coxph(
    formula = stats::as.formula(
      paste0(
        dplyr::if_else(
          is.na(time2),
          true = "survival::Surv(time = .time, ",
          false = "survival::Surv(time = .time_orig, time2 = .time2, "
        ),
        "event = .event) ~ .exposure ",
        confounders
      )
    ),
    data = data,
    weights = data$.weights,
    robust = coxph_robust,
    id = data$.id
  ) |>
    broom::tidy(
      conf.int = TRUE,
      conf.level = ci,
      exponentiate = TRUE
    ) |>
    format_regression_results(
      data = data,
      suppress = "event",
      is_trend = is_trend,
      multiply = 1,
      digits = digits,
      ratio_digits_decrease = ratio_digits_decrease,
      pattern = pattern,
      xlevels = xlevels,
      reference = 1,
      nmin = nmin,
      to = to,
      reference_label = reference
    )
}
