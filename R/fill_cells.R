#' Select cell fill function
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param time2 Second time variable
#' @param exposure Exposure variable
#' @param stratum Effect modifier level
#' @param confounders String of covariates
#' @param weights Name of weights variable
#' @param type Type of statistic quested from table_count
#' @param trend Continuous (trend) exposure variable
#' @param digits Number of digits to round an individual estimate to
#' @param nmin Minimum number of observations/events to display an estimate
#' @param na_rm Suppress observations with missing outcome
#' @param ci Confidence interval limits
#' @param factor Factor for rates. Defaults to 1000.
#' @param risk_percent Show risks and risk differences as percentages?
#' @param diff_digits Number of digits to round difference estimates to
#' @param risk_digits Number of digits to round risk/cumulative incidences to
#' @param ratio_digits Number of digits to round ratio estimates to
#' @param ratio_digits_decrease Fewer digits for elevated ratios
#' @param rate_digits Number of digits to round rate estimates to
#' @param to Separator for mean/difference confidence intervals.
#' @param outcome Outcome variable
#' @param effect_modifier Effect modifier variable
#' @param arguments Optional list of arguments passed on to estimators
#' @param reference Label of reference category
#' @param exposure_levels How to handle empty or missing exposure levels
#'
#' @return Tibble
#' @noRd
fill_cells <- function(
    data,
    event,
    time,
    time2,
    outcome,
    exposure,
    effect_modifier,
    stratum,
    confounders,
    weights,
    type,
    trend,
    digits,
    nmin,
    na_rm,
    ci,
    to,
    reference,
    arguments,
    factor,
    risk_percent,
    diff_digits,
    risk_digits,
    ratio_digits,
    ratio_digits_decrease,
    rate_digits,
    exposure_levels) {
  if (is.na(exposure) | exposure == "") {
    data <- data %>%
      dplyr::mutate(.exposure = "Overall")
  } else {
    data$.exposure <- data[[exposure]]

    # Check that exposure is categorical
    if (!(class(data %>% dplyr::pull(.data$.exposure))[1] %in%
      c("factor", "character", "logical"))) {
      warning(
        paste0(
          "Exposure variable '", exposure,
          "' is not categorical (factor, character, or logical). ",
          "Its type was changed to 'factor' but the result may be ",
          "undesirable, e.g., if the variable is actually continuous ",
          "and thus has many levels."
        )
      )
    }

    if (is.logical(data$.exposure) & exposure_levels == "all") {
      data$.exposure <- factor(
        data$.exposure,
        levels = c(FALSE, TRUE)
      )
    }

    if (!is.factor(data$.exposure) |
      exposure_levels != "all") { # "nona", "noempty"
      data$.exposure <- factor(data$.exposure)
    } # if factor, drops empty levels

    if (exposure_levels == "nona") {
      data <- data %>%
        dplyr::filter(!is.na(.data$.exposure))
    }
  }

  # Check that trend variable, if given, is continuous
  if (!is.na(trend)) {
    if (trend != "") {
      if (!(trend %in% names(data))) {
        stop(
          paste0(
            "Trend variable '",
            trend,
            "' is not valid for the dataset."
          )
        )
      }
      data <- data %>%
        dplyr::rename(.trend = dplyr::one_of(trend))
      if (
        !is.numeric(
          data %>%
            dplyr::pull(.data$.trend)
        )) {
        stop(
          paste0(
            "Trend variable '",
            trend,
            "' is not continuous (numeric)."
          )
        )
      }
    }
  }

  if (type == "" |
    type == "blank" |
    is.na(type)) {
    if ((is.na(exposure) | exposure == "") &
      (is.na(trend) | trend == "")) {
      return(tibble::tibble(res = ""))
    }
    if (is.na(trend) | trend == "") {
      return(
        tibble::tibble(
          .exposure = data %>%
            dplyr::pull(.data$.exposure) %>%
            levels(),
          res = ""
        )
      )
    }
    return(
      tibble::tibble(
        .exposure = c(
          data %>%
            dplyr::pull(.data$.exposure) %>%
            levels(),
          "Trend"
        ),
        res = ""
      )
    )
  }

  if (is.na(nmin)) {
    nmin <- 0
  }
  if (is.na(ci)) {
    ci <- 0.95
  }
  if (is.na(confounders)) {
    confounders <- ""
  }

  data_prep <- prepare_data(
    data = data,
    is_trend = FALSE,
    na_rm = na_rm,
    outcome = outcome,
    event = event,
    time = time,
    time2 = time2,
    weights = weights,
    type = type,
    effectmodifier = effect_modifier,
    effectmodifier_level = stratum
  )

  estimator <- switch(
    EXPR = type,
    "outcomes" = ,
    "outcomes/total" = ,
    "cases/controls" = ,
    "risk" = ,
    "risk (ci)" = ,
    "outcomes (risk)" = ,
    "outcomes/total (risk)" = "outcome_binary",

    "total" = ,
    "mean" = ,
    "mean (sd)" = ,
    "sd" = ,
    "mean (ci)" = ,
    "geomean" = ,
    "median" = ,
    "median (iqr)" = ,
    "range" = "outcome_continuous",

    "events" = ,
    "time" = ,
    "events/time" = ,
    "events/total" = ,
    "rate" = ,
    "rate (ci)" = ,
    "events/time (rate)" = ,
    "medsurv" = ,
    "medsurv (ci)" = ,
    "medfu" = ,
    "medfu (iqr)" = ,
    "maxfu" = ,
    "surv" = ,
    "surv (ci)" = ,
    "cuminc" = ,
    "cuminc (ci)" = "event_time",

    "survdiff" = ,
    "survdiff_joint" = ,
    "cumincdiff" = ,
    "cumincdiff_joint" = ,
    "survratio" = ,
    "survratio_joint" = ,
    "cumincratio" = ,
    "cumincratio_joint" = "survdiff",

    "hr" = ,
    "hr_joint" = "regress_cox",

    "rr" = ,
    "rr_joint" = ,
    "rd" = ,
    "rd_joint" = ,
    "or" = ,
    "or_joint" = "regress_binary",

    "diff_joint" = ,
    "diff" = ,
    "irrrob" = ,
    "irrrob_joint" = ,
    "irr_joint" = ,
    "irr" = ,
    "fold_joint" = ,
    "fold" = ,
    "foldlog_joint" = ,
    "foldlog" = ,
    "quantreg_joint" = ,
    "quantreg" = "regress_continuous",

    # default choice- check if custom function is available:
    {
      if (!exists(paste0("estimate_", type), mode = "function")) {
        stop(
          paste0(
            "An estimator type = '",
            type,
            "' is not implemented by default, and no custom function named ",
            "'estimate_",
            type,
            "' is currently available."
          )
        )
      } else {
        type
      }
    }
  )

  res_cat <- do.call(
    what = paste0("estimate_", estimator),
    args = list(
      data = data_prep$data,
      type = type,
      event = event,
      time = time,
      time2 = time2,
      outcome = outcome,
      exposure = exposure,
      effectmodifier = effect_modifier,
      effectmodifier_level = stratum,
      confounders = confounders,
      weights = weights,
      digits = digits,
      nmin = nmin,
      na_rm = na_rm,
      ci = ci,
      xlevels = data_prep$xlevels,
      pattern = data_prep$pattern,
      diff_digits = diff_digits,
      risk_digits = risk_digits,
      ratio_digits = ratio_digits,
      ratio_digits_decrease = ratio_digits_decrease,
      rate_digits = rate_digits,
      risk_percent = risk_percent,
      to = to,
      reference = reference,
      factor = factor,
      arguments = arguments,
      event_type = data_prep$event_type,
      is_trend = FALSE
    )
  )

  if (setequal(
    res_cat,
    tibble::tibble(
      .exposure = NA_character_,
      res = NA_character_,
      .rows = 0
    )
  )) {
    res_cat <- tibble::tibble(
      .exposure = "Overall",
      res = ""
    )
  }

  if (is.na(trend) | trend == "") {
    return(res_cat)
  } else {
    data_prep <- data %>%
      dplyr::mutate(.exposure = .data$.trend) %>%
      prepare_data(
        is_trend = TRUE,
        na_rm = na_rm,
        outcome = outcome,
        event = event,
        time = time,
        time2 = time2,
        weights = weights,
        type = type,
        effectmodifier = effect_modifier,
        effectmodifier_level = stratum
      )
    res_trend <- do.call(
      what = paste0("estimate_", estimator),
      args = list(
        data = data_prep$data,
        type = type,
        event = event,
        time = time,
        time2 = time2,
        outcome = outcome,
        exposure = trend,
        effectmodifier = effect_modifier,
        effectmodifier_level = stratum,
        confounders = confounders,
        weights = weights,
        digits = digits,
        nmin = nmin,
        na_rm = na_rm,
        ci = ci,
        xlevels = data_prep$xlevels,
        pattern = data_prep$pattern,
        diff_digits = diff_digits,
        risk_digits = risk_digits,
        ratio_digits = ratio_digits,
        ratio_digits_decrease = ratio_digits_decrease,
        rate_digits = rate_digits,
        risk_percent = risk_percent,
        to = to,
        reference = reference,
        factor = factor,
        arguments = arguments,
        event_type = data_prep$event_type,
        is_trend = TRUE
      )
    )
    if (setequal(
      res_cat,
      tibble::tibble(
        .exposure = "Overall",
        res = ""
      )
    )) {
      return(res_trend)
    } else {
      return(
        dplyr::bind_rows(
          res_cat,
          res_trend %>%
            dplyr::mutate(
              dplyr::across(
                .cols = dplyr::any_of(".exposure"),
                .fns = as.character
              )
            )
        )
      )
    }
  }
}
