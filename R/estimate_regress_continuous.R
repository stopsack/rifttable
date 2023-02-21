#' Point Estimate and CI From Regression Models for Continuous Outcomes
#'
#' @param data Data set
#' @param confounders String of covariates
#' @param risk_percent Display risk differences as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param type Estimand
#' @param ratio_digits Digits for ratios
#' @param diff_digits Digits for differences
#' @param nmin Suppress counts below
#' @param exposure Name of exposure variable
#' @param na_rm Remove observations with missing outcome data
#' @param arguments List of optional arguments
#' @param ci Confidence interval width
#' @param pattern Regex pattern for removing regression terms
#' @param xlevels Strata of exposure variable
#' @param reference Label for reference category
#' @param ... Additional arguments
#'
#' @return Tibble
#' @noRd
estimate_regress_continuous <- function(
    data,
    type,
    exposure,
    confounders,
    digits,
    risk_percent,
    ratio_digits,
    diff_digits,
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
    default = dplyr::if_else(
      type %in% c("diff", "diff_joint", "quantreg", "quantreg_joint"),
      true = diff_digits,
      false = ratio_digits))
  tau <- find_argument(
    arguments = arguments,
    which_argument = "tau",
    is_numeric = TRUE,
    default = 0.5)

  switch(
    EXPR = type,
    diff_joint =,
    diff = {
      stats::lm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders)),
        data = data) %>%
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = FALSE)
    },
    irrrob =,
    irrrob_joint = {
      if (!requireNamespace("sandwich", quietly = TRUE)) {
        stop(
          paste0(
            "The package \"sandwich\" must be installed to estimate robust ",
            "standard errors for type = '",
            type,
            "'.\nInstall with:  install.packages(\"sandwich\")"),
          call. = FALSE)
      }
      fit <- stats::glm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders)),
        family = stats::poisson(link = "log"),
        data = data)
      fit %>%
        broom::tidy(
          conf.int = FALSE,
          exponentiate = FALSE) %>%
        dplyr::mutate(
          std.error = sqrt(diag(sandwich::vcovHC(
            fit,
            type = "HC0"))),
          statistic = .data$estimate / .data$std.error,
          conf.low = .data$estimate -
            stats::qnorm(1 - (1 - ci) / 2) *
            .data$std.error,
          conf.high = .data$estimate +
            stats::qnorm(1 - (1 - ci) / 2) *
            .data$std.error) %>%
        dplyr::mutate_at(
          .vars = dplyr::vars(
            .data$estimate,
            .data$conf.low,
            .data$conf.high),
          .funs = exp)
    },
    irr_joint =,
    irr = {
      stats::glm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders)),
        family = stats::poisson(link = "log"),
        data = data) %>%
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = TRUE)
    },
    fold_joint =,
    fold = {
      stats::glm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders)),
        family = stats::gaussian(link = "log"),
        data = data) %>%
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = TRUE)
    },
    foldlog_joint =,
    foldlog = {
      stats::lm(formula = stats::as.formula(
        paste(
          "log(.outcome) ~ .exposure",
          confounders)),
        data = data) %>%
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci) %>%
        dplyr::mutate_at(
          .vars = dplyr::vars(
            .data$estimate,
            .data$conf.low,
            .data$conf.high),
          .funs = exp)
    },
    quantreg_joint =,
    quantreg = {
      if (!requireNamespace("quantreg", quietly = TRUE)) {
        stop(
          paste0(
            "The package \"quantreg\" must be installed to use type = '",
            type,
            "'.\nInstall with:  install.packages(\"quantreg\")"),
          call. = FALSE)
      }
      quantreg::rq(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders)),
        tau = tau,
        method = "fn",
        data = data) %>%
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci)
    }) %>%
    format_regression_results(
      data = data,
      suppress = "total",
      is_trend = is_trend,
      multiply = 1,
      digits = digits,
      pattern = pattern,
      xlevels = xlevels,
      reference = dplyr::if_else(
        type %in% c("diff", "diff_joint", "quantreg", "quantreg_joint"),
        true = 0,
        false = 1),
      nmin = nmin,
      to = to,
      reference_label = reference)
}
