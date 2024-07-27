#' Point Estimate and CI From Binary Outcome Regression Models
#'
#' @param data Data set
#' @param confounders String of covariates
#' @param risk_percent Display risk differences as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param type Estimand
#' @param risk_digits Digits for risks
#' @param ratio_digits Digits for ratios
#' @param ratio_digits_decrease Fewer digits for elevated ratios
#' @param nmin Suppress counts below
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
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
estimate_regress_binary <- function(
    data,
    type,
    exposure,
    outcome,
    confounders,
    digits,
    risk_percent,
    risk_digits,
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
  check_outcome(
    data = data,
    type = type,
    outcome = outcome,
    outcome_type = "binary"
  )
  digits <- find_rounding_digits(
    digits = digits,
    default = dplyr::if_else(
      type %in% c("rd", "rd_joint"),
      true = risk_digits,
      false = ratio_digits
    )
  )
  if (type %in% c("rd", "rd_joint")) {
    ratio_digits_decrease <- NULL
  }
  bootrepeats <- find_argument(
    arguments = arguments,
    which_argument = "bootrepeats",
    is_numeric = TRUE,
    default = 1000
  )
  if (type %in% c("rd", "rd_joint")) {
    acceptable_approaches <- as.character(as.list(args(risks::riskdiff))$approach)
  } else {
    acceptable_approaches <- as.character(as.list(args(risks::riskratio))$approach)
  }
  approach <- find_argument(
    arguments = arguments,
    which_argument = "approach",
    is_numeric = FALSE,
    acceptable = acceptable_approaches[!(acceptable_approaches %in% c("c", "all"))],
    default = "auto"
  )

  switch(
    EXPR = type,
    rr_joint = ,
    rr = {
      risks::riskratio(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders
          )
        ),
        data = data,
        approach = approach
      ) %>%
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = TRUE,
          bootrepeats = bootrepeats
        )
    },
    rd_joint = ,
    rd = {
      risks::riskdiff(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders
          )
        ),
        data = data,
        approach = approach
      ) %>%
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = FALSE,
          bootrepeats = bootrepeats
        )
    },
    or_joint = ,
    or = {
      stats::glm(
        formula = stats::as.formula(
          paste(
            ".outcome ~ .exposure",
            confounders
          )
        ),
        family = stats::binomial(link = "logit"),
        data = data
      ) %>%
        broom::tidy(
          conf.int = TRUE,
          conf.level = ci,
          exponentiate = TRUE
        )
    },
    stop(paste0("Invalid estimator type = '", type, "'."))
  ) %>%
    format_regression_results(
      data = data,
      suppress = "binary",
      is_trend = is_trend,
      multiply = dplyr::if_else(
        risk_percent == TRUE & type %in% c("rd", "rd_joint"),
        true = 100,
        false = 1
      ),
      digits = digits,
      ratio_digits_decrease = ratio_digits_decrease,
      pattern = pattern,
      xlevels = xlevels,
      reference = dplyr::if_else(
        type %in% c("rd", "rd_joint"),
        true = 0,
        false = 1
      ),
      nmin = nmin,
      to = to,
      reference_label = reference,
      percent = risk_percent & type %in% c("rd", "rd_joint"),
      conflimit_check = type %in% c("rr", "rd", "rd_joint", "rr_joint")
    )
}
