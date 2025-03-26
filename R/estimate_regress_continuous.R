#' Point Estimate and CI From Regression Models for Continuous Outcomes
#'
#' @param data Data set
#' @param confounders String of covariates
#' @param risk_percent Display risk differences as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param type Estimand
#' @param outcome Outcome variable
#' @param ratio_digits Digits for ratios
#' @param ratio_digits_decrease Fewer digits for elevated ratios
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
    outcome,
    exposure,
    confounders,
    digits,
    risk_percent,
    ratio_digits,
    ratio_digits_decrease,
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
  check_outcome(
    data = data,
    type = type,
    outcome = outcome,
    outcome_type = "continuous"
  )
  digits <- find_rounding_digits(
    digits = digits,
    default = dplyr::if_else(
      type %in% c("diff", "diff_joint", "quantreg", "quantreg_joint"),
      true = diff_digits,
      false = ratio_digits
    )
  )
  if (type %in% c("diff", "diff_joint", "quantreg", "quantreg_joint")) {
    ratio_digits_decrease <- NULL
  }
  tau <- find_argument(
    arguments = arguments,
    which_argument = "tau",
    is_numeric = TRUE,
    default = 0.5
  )

  switch(
    EXPR = type,
    diff_joint = ,
    diff = {
      stats::lm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders
          )
        ),
        data = data
      ) |>
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = FALSE
        )
    },
    irrrob = ,
    irrrob_joint = {
      if (!is_package_installed("sandwich")) {
        stop(
          paste0(
            "The package \"sandwich\" must be installed to estimate robust ",
            "standard errors for type = '",
            type,
            "'.\nInstall with:  install.packages(\"sandwich\")"
          ),
          call. = FALSE
        )
      }
      fit <- suppressWarnings(stats::glm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders
          )
        ),
        family = stats::poisson(link = "log"),
        data = data
      ))
      fit |>
        broom::tidy(
          conf.int = FALSE,
          exponentiate = FALSE
        ) |>
        dplyr::mutate(
          std.error = sqrt(diag(sandwich::vcovHC(
            fit,
            type = "HC0"
          ))),
          statistic = .data$estimate / .data$std.error,
          conf.low = .data$estimate -
            stats::qnorm(1 - (1 - ci) / 2) *
              .data$std.error,
          conf.high = .data$estimate +
            stats::qnorm(1 - (1 - ci) / 2) *
              .data$std.error
        ) |>
        dplyr::mutate_at(
          .vars = c(
            "estimate",
            "conf.low",
            "conf.high"
          ),
          .funs = exp
        )
    },
    irr_joint = ,
    irr = {
      stats::glm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders
          )
        ),
        family = stats::poisson(link = "log"),
        data = data
      ) |>
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = TRUE
        )
    },
    fold_joint = ,
    fold = {
      stats::glm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders
          )
        ),
        family = stats::gaussian(link = "log"),
        data = data
      ) |>
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = TRUE
        )
    },
    foldlog_joint = ,
    foldlog = {
      stats::lm(
        formula = stats::as.formula(
          paste(
            "log(.outcome) ~ .exposure",
            confounders
          )
        ),
        data = data
      ) |>
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci
        ) |>
        dplyr::mutate_at(
          .vars = c(
            "estimate",
            "conf.low",
            "conf.high"
          ),
          .funs = exp
        )
    },
    quantreg_joint = ,
    quantreg = {
      if (!is_package_installed("quantreg")) {
        stop(
          paste0(
            "The package \"quantreg\" must be installed to use type = '",
            type,
            "'.\nInstall with:  install.packages(\"quantreg\")"
          ),
          call. = FALSE
        )
      }
      quantreg::rq(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders
          )
        ),
        tau = tau,
        method = "fn",
        data = data
      ) |>
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci
        )
    }
  ) |>
    format_regression_results(
      data = data,
      suppress = "total",
      is_trend = is_trend,
      multiply = 1,
      digits = digits,
      ratio_digits_decrease = ratio_digits_decrease,
      pattern = pattern,
      xlevels = xlevels,
      reference = dplyr::if_else(
        type %in% c("diff", "diff_joint", "quantreg", "quantreg_joint"),
        true = 0,
        false = 1
      ),
      nmin = nmin,
      to = to,
      reference_label = reference
    )
}
