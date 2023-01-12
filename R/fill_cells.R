#' Select cell fill function
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param time2 Second time variable
#' @param exposure Exposure variable
#' @param effectmodifier Effect modifier variable
#' @param stratum Effect modifier level
#' @param confounders String of covariates
#' @param type Type of statistic quested from table_count
#' @param trend Continuous (trend) exposure variable
#' @param digits Number of digits to round an individual estimate to
#' @param nmin Minimum number of observations/events to display an estimate
#' @param factor Factor for rates. Defaults to 1000.
#' @param risk_percent Show risks and risk differences as percentages?
#' @param diff_digits Number of digits to round difference estimates to
#' @param risk_digits Number of digits to round risk/cumulative incidences to
#' @param ratio_digits Number of digits to round ratio estimates to
#' @param rate_digits Number of digits to round rate estimates to
#' @param to Separator for mean/difference confidence intervals.
#'
#' @return Tibble
#' @noRd
fill_cells <- function(data, event, time, time2, outcome,
                       exposure, effect_modifier, stratum, confounders,
                       type, trend, digits, nmin, factor, risk_percent,
                       diff_digits, risk_digits, ratio_digits, rate_digits,
                       to, custom_fn) {
  if(is.na(exposure)) {
    data <- data %>% dplyr::mutate(.exposure = "Overall")
  } else {
    data <- data %>% dplyr::rename(.exposure = dplyr::one_of(exposure))

    # Check that exposure is categorical
    if(!(class(data %>% pull(.data$.exposure))[1] %in% c("factor", "character",
                                                         "logical")))
      warning(paste0("Exposure variable '", exposure,
                     "' is not categorical (factor, character, or logical). ",
                     "Its type was changed to 'factor' but the result may be ",
                     "undesirable, e.g., if the variable is actually
                     continuous and thus has many levels."))
    data$.exposure <- factor(data$.exposure)
  }

  # Check that trend variable, if given, is continuous
  if(!is.na(trend)) {
    if(!(trend %in% names(data)))
      stop(paste0("Trend variable '", trend, "' is not valid for the dataset."))
    data <- data %>% dplyr::rename(.trend = dplyr::one_of(trend))
    if(class(data %>% pull(.data$.trend))[1] != "numeric")
      stop(paste0("Trend variable '", trend,
                  "' is not continuous (numeric)."))
  }

  if(type == "" | type == "blank") {
    if(is.na(exposure)) {
      return(tibble::tibble(.exposure = "Overall", res = ""))
    } else {
      return(tibble::tibble(.exposure = data %>%
                              dplyr::pull(.data$.exposure) %>%
                              levels(),
                            res = ""))
    }
  }

  # Check that time and event variable exist, if needed
  if(stringr::str_detect(
    string = type,
    pattern = "events|hr|rate|time|medsurv|medfu|maxfu|cuminc|rmtl|rmtdiff|^surv")) {
    if(!(event %in% names(data)))
      stop(paste0("Survival data using type = '", type, "' requested, but ",
                  "event variable '", event, "' is not valid for the dataset."))
    if(!(time %in% names(data)))
      stop(paste0("Survival data using type = '", type, "' requested, but ",
                  "time variable '", time, "' is not valid for the dataset."))
    if(!is.na(time2))
      if(!(time2 %in% names(data)))
        stop(paste0("Survival data using type = '", type, "' with enter and ",
                    "exit times was requested, but the second time variable '",
                    time2, "' is not valid for the dataset."))
  }

  # Check that outcome variable exists, if needed
  if(stringr::str_detect(
    string = type,
    pattern = "outcomes|^diff|mean|median|risk|^rr|rd|irr|fold|foldlog|or|cases|quantreg|^sd|range")) {
    if(!(outcome %in% names(data)))
      stop(paste0("Using type = '", type, "' requires an outcome variable, ",
                  "but the variable '", outcome,
                  "' is not valid for the dataset."))
    # Check that outcome is binary
    outcomevar <- data %>%
      dplyr::select(outcome = dplyr::one_of(outcome)) %>%
      dplyr::pull(outcome)
    if(!(stringr::str_detect(
      string = type,
      pattern = "^diff|mean|median|irr|fold|foldlog|quantreg|^sd|range")))
      if(!(all(outcomevar %in% c(0, 1, NA)) |
           all(outcomevar %in% c(FALSE, TRUE, NA))))
        stop(paste0("Outcome variable '", outcome,
                    "' must be binary with levels c(0, 1) or c(FALSE, TRUE)."))
  }

  if(!is.na(digits)) {
    if(!is.numeric(digits))
      stop(paste0("'digits' value for rounding, if given, must be numeric. '",
                  digits, "'is not numeric."))
    # set rounding digits, if not provided per row:
  } else {
    digits <- dplyr::case_when(
      stringr::str_detect(string = type,
                          pattern = "hr|rr|irr|fold|foldlog|or") ~
        ratio_digits[1],
      stringr::str_detect(
        string = type,
        pattern = "^diff|mean|median|quantreg|medsurv|medfu|maxfu|rmtl|rmtdiff|^sd|range") ~
        diff_digits[1],
      stringr::str_detect(
        string = type,
        pattern = "rd|risk|cuminc|^surv") ~
        risk_digits[1],
      stringr::str_detect(string = type,
                          pattern = "rate") ~
        rate_digits[1],
      stringr::str_detect(string = type,
                          pattern = "time$") ~
        0,
      TRUE ~ 4)
  }

  if(stringr::str_detect(
    string = type,
    pattern = "hr|^rr|rd|irr|fold|foldlog|^diff|or|quantreg|rmtl|rmtdiff|survdiff|cumincdiff")) {
    if(is.na(exposure) &   # no exposure variable given
       !stringr::str_detect(string = type,
                            pattern = "rmtl")) {
      return(tibble(.exposure = "Overall", res = ""))
    } else {
      res_cat <- table_regress(data = data,
                               estimand = type,
                               event = event,
                               time = time,
                               time2 = time2,
                               outcome = outcome,
                               effectmodifier = effect_modifier,
                               effectmodifier_level = stratum,
                               confounders = confounders,
                               digits = digits,
                               risk_percent = risk_percent,
                               to = to,
                               nmin = nmin)
      if(!is.na(trend)) {
        dplyr::bind_rows(res_cat,
                         table_regress(
                           data = data %>%
                             dplyr::mutate(.exposure = .data$.trend),
                           estimand = type,
                           event = event,
                           time = time,
                           time2 = time2,
                           outcome = outcome,
                           effectmodifier = effect_modifier,
                           effectmodifier_level = stratum,
                           confounders = confounders,
                           digits = digits,
                           risk_percent = risk_percent,
                           to = to,
                           is_trend = TRUE,
                           nmin = nmin))
      } else {
        res_cat
      }
    }
  } else {
    if(stringr::str_detect(string = type, pattern = "^custom")) {
      if(is.null(custom_fn))
        stop(paste0("Custom function requested via type = '", type, "', but ",
                    "no table2(..., custom = <function(s)>) was provided."))
      type <- stringr::str_extract(string = type,
                                   pattern = "^custom[:digit:]{0,2}") %>%
        stringr::str_remove_all(pattern = "^custom|\\h")
      type <- suppressWarnings(as.numeric(type))
      if(is.na(type)) {
        if(is.list(custom_fn))
          stop(paste0("More than one custom function provided via",
                      "table2(..., custom = <list>), but 'type' ('", type,
                      "') did not indicate which one to run."))
        if(!is.function(custom_fn))
          stop("The provided argument for 'custom' is not a function.")
        custom_fn(data)
      } else {
        if(!is.list(custom_fn) & type != 1)
          stop(paste("One or fewer custom functions provided via",
                     "table2(..., custom = <list>), but 'type' ('", type,
                     "') requested a specific custom function."))
        if(!is.function(custom_fn[[type]]))
          stop("The provided argument for 'custom' is not a list of functions.")
        res_cat <- custom_fn[[type]](
          data,
          event = event,
          time = time,
          time2 = time2,
          outcome = outcome,
          effectmodifier = effect_modifier,
          effectmodifier_level = stratum,
          confounders = confounders,
          type = type,
          factor = factor,
          digits = digits,
          risk_percent = risk_percent,
          to = to,
          nmin = nmin)
        if(!is.na(trend)) {
          dplyr::bind_rows(
            res_cat,
            custom_fn[[type]](
              data %>%
                dplyr::mutate(.exposure = .data$.trend),
              event = event,
              time = time,
              time2 = time2,
              outcome = outcome,
              effectmodifier = effect_modifier,
              effectmodifier_level = stratum,
              confounders = confounders,
              type = type,
              factor = factor,
              digits = digits,
              risk_percent = risk_percent,
              to = to,
              is_trend = TRUE,
              nmin = nmin))
        } else {
          res_cat
        }
      }
    } else {
      table_counts(data = data,
                   event = event,
                   time = time,
                   time2 = time2,
                   outcome = outcome,
                   effectmodifier = effect_modifier,
                   effectmodifier_level = stratum,
                   type = type,
                   factor = factor,
                   digits = digits,
                   risk_percent = risk_percent,
                   to = to,
                   nmin = nmin)
    }
  }
}
