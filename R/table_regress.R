#' Get point estimate and CI from regression models
#'
#' @param data Dataset
#' @param estimand Regression estimand
#' @param event Event variable
#' @param time Time variable
#' @param time2 Second time variable
#' @param outcome Outcome variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#' @param risk_percent Display risk differences as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#'
#' @return Tibble
#' @noRd
table_regress <- function(data, estimand, event, time, time2, outcome,
                          effectmodifier = NULL, effectmodifier_level = NULL,
                          confounders = "", risk_percent = FALSE, digits = 2,
                          is_trend = FALSE, nmin = NA,
                          to = "-") {
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  if(stringr::str_detect(string = estimand, pattern = "_joint") &
     is_trend == FALSE) {  # trends must be stratum-specific
    if(missing(effectmodifier) | missing(effectmodifier_level))
      stop(paste0("Effect modifier and stratum must be specified for joint",
                  "model ('", estimand, "')."))
    if(is.na(effectmodifier) | is.null(effectmodifier) |
       is.null(effectmodifier_level))
      stop(paste0("Effect modifier and stratum must be specified for joint",
                  "model ('", estimand, "')."))
    pattern <- paste0(".exposure[:digit:]{1,2}__", effectmodifier_level,
                      "__[:digit:]{1,2}__")
    data <- data %>%
      dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
      dplyr::filter(!is.na(.data$.effectmod)) %>%
      dplyr::mutate(.effectmod = factor(.data$.effectmod))
    xlevels_indices <- 1:length(xlevels)
    names(xlevels_indices) <- xlevels
    emlevels <- data %>%
      dplyr::pull(.data$.effectmod) %>%
      factor() %>%
      levels()
    emlevels_indices <- 1:length(emlevels)
    names(emlevels_indices) <- emlevels
    data <- data %>%
      dplyr::mutate(.exposure = paste(emlevels_indices[.data$.effectmod],
                                      .data$.effectmod,
                                      xlevels_indices[.data$.exposure],
                                      .data$.exposure,
                                      sep = "__"))
  } else {
    pattern <- ".exposure"
    if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
      if(!is.null(effectmodifier_level) & !(is.null(effectmodifier) |
                                            is.na(effectmodifier))) {
        data <- data %>%
          dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
          dplyr::filter(.data$.effectmod %in% effectmodifier_level)
      }
    }
  }

  # Find exposure categories with sparse data:
  if(is.na(nmin))
    nmin <- 0
  count_per_stratum <- counts_per_stratum(
    data = data,
    event = event,
    estimand = estimand,
    is_trend = is_trend)

  # Select specific quantiles for quantreg, if provided:
  if(stringr::str_detect(string = estimand, pattern = "quantreg")) {
    tau <- stringr::str_remove_all(string = estimand,
                                   pattern = "quantreg|_joint|\\h")
    if(stringr::str_length(string = tau) > 0)
      tau <- as.numeric(tau)
    else
      tau <- 0.5
    if(is.na(tau))
      stop(paste0("The supplied quantile is not a valid numeric value: '",
                  estimand, "'."))
    if(stringr::str_detect(string = estimand, pattern = "_joint"))
      estimand <- "quantreg_joint"
    else
      estimand <- "quantreg"
  }

  if(stringr::str_detect(string = estimand, pattern = "^rd|^rr")) {
    # Select bootstrap repeats, if provided:
    bootrepeats <- as.numeric(stringr::str_extract(string = estimand,
                                                   pattern = "[:digit:]+"))
    if(!is.na(bootrepeats))
      bootrepeats <- max(bootrepeats, 50)
    # Select model fitting approach for riskdiff or riskratio, if provided:
    approach <- stringr::str_remove_all(
      string = estimand,
      pattern = "^rd|^rr|_joint|\\h|[:digit:]")
    if(stringr::str_length(string = approach) == 0)
      approach <- "auto"
    if(stringr::str_detect(string = estimand, pattern = "^rd")) {
      possible_approaches <- as.character(as.list(args(
        risks::riskdiff))$approach)
    } else {
      possible_approaches <- as.character(as.list(args(
        risks::riskratio))$approach)
    }
    possible_approaches <- possible_approaches[!(possible_approaches %in%
                                                   c("c", "all"))]
    if(!(approach %in% possible_approaches)) {
      warning(paste0("Fitting a risks model via approach '",
                     approach,
                     "' is not implemented. Possible approaches are: ",
                     paste(possible_approaches, sep = ", ", collapse = ", "),
                     ". Defaulting to 'auto'."))
      approach <- "auto"
    }
    if(stringr::str_detect(string = estimand, pattern = "_joint")) {
      estimand <- paste0(stringr::str_extract(string = estimand,
                                              pattern = "rd|rr"), "_joint")
    } else {
      estimand <- stringr::str_extract(string = estimand, pattern = "rd|rr")
    }
  }
  if(exists("bootrepeats")) {
    if(is.na(bootrepeats))
      bootrepeats <- 1000  # default if not provided. used for risks models
  } else {
    bootrepeats <- 1000
  }

  # Extract follow-up time for survival data:
  if(stringr::str_detect(string = estimand,
                         pattern = "survdiff|cumincdiff")) {
    timepoint <- stringr::str_remove_all(
      string = estimand,
      pattern = "_joint|survdiff|cumincdiff|\\(ci\\)|\\h")
    estimand_joint <- stringr::str_extract(string = estimand,
                                           pattern = "_joint|")
    estimand <- stringr::str_extract(
      string = estimand,
      pattern = "cumincdiff|survdiff")
    surv_cuminc <- dplyr::if_else(estimand == "survdiff",
                                  true = "survival", false = "cuminc")
    estimand <- paste0(estimand, estimand_joint)
    if(stringr::str_length(string = timepoint) > 0) {
      if(is.na(as.numeric(timepoint)))
        stop(paste0("The supplied time horizon is not ",
                    "a valid numeric value: '",
                    timepoint, "'."))
      timepoint <- as.numeric(timepoint)
    } else {
      stop("Must provide a time horizon for this survival analysis.")
    }
  }

  if(is.na(confounders))
    confounders <- ""

  multiply <- 1
  fit <- switch(EXPR = estimand,
                hr_joint =,
                hr = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  if(is.na(time2))
                    survival::coxph(formula = stats::as.formula(
                      paste0("survival::Surv(time = ", time, ",
                             event = ", event, ") ~ .exposure",
                             confounders)),
                      data = data)
                  else
                    survival::coxph(formula = stats::as.formula(
                      paste0("survival::Surv(time = ", time, ", time2 =", time2,
                             ", event = ", event, ") ~ .exposure",
                             confounders)),
                      data = data)
                },
                rr_joint =,
                rr = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  risks::riskratio(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    data = data, approach = approach)
                },
                rd_joint =,
                rd = {
                  reference <- 0
                  exponent <- FALSE
                  multiply <- dplyr::if_else(risk_percent == TRUE,
                                      true = 100, false = 1)
                  to <- dplyr::if_else(is.null(to), true = " to ", false = to)
                  risks::riskdiff(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    data = data, approach = approach)
                },
                diff_joint =,
                diff = {
                  reference <- 0
                  exponent <- FALSE
                  to <- dplyr::if_else(is.null(to), true = " to ", false = to)
                  stats::lm(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    data = data)
                },
                irrrob =,
                irrrob_joint =,
                irr_joint =,
                irr = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  stats::glm(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    family = stats::poisson(link = "log"),
                    data = data)
                },
                fold_joint =,
                fold = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  stats::glm(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    family = stats::gaussian(link = "log"),
                    data = data)
                },
                foldlog_joint =,
                foldlog = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  stats::lm(formula = stats::as.formula(
                    paste0("log(", outcome, ") ~ .exposure ", confounders)),
                    data = data)
                },
                or_joint =,
                or = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  stats::glm(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    family = stats::binomial(link = "logit"),
                    data = data)
                },
                quantreg_joint =,
                quantreg = {
                  reference <- 0
                  exponent <- FALSE
                  to <- dplyr::if_else(is.null(to), true = " to ", false = to)
                  quantreg::rq(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    tau = tau, method = "fn",
                    data = data)
                },
                survdiff =,
                survdiff_joint = ,
                cumincdiff = ,
                cumincdiff_joint = {
                  reference <- 0
                  exponent <- FALSE
                  to <- dplyr::if_else(is.null(to), true = " to ", false = to)
                  multiply <- dplyr::if_else(risk_percent == TRUE,
                                      true = 100, false = 1)
                  if(is.na(time2))
                    survdiff_ci(formula = stats::as.formula(
                      paste0("survival::Surv(time = ", time, ",
                             event = ", event, ") ~ .exposure")),
                      data = data, time = timepoint, estimand = surv_cuminc)
                  else
                    survdiff_ci(formula = stats::as.formula(
                      paste0("survival::Surv(time = ", time, ", time2 =", time2,
                             ", event = ", event, ") ~ .exposure")),
                      data = data, time = timepoint, estimand = surv_cuminc)
                },
                stop(paste0("Estimand '", estimand, "' is not implemented.")))

  fit <- switch(
    EXPR = estimand,
    # tidy.rq does not like the "exponentiate" argument:
    quantreg =,
    quantreg_joint = broom::tidy(fit, conf.int = TRUE),
    # tidy.lm ignores "exponentiate":
    foldlog =,
    foldlog_joint = {
      broom::tidy(fit, conf.int = TRUE) %>%
        dplyr::mutate_at(.vars = dplyr::vars(.data$estimate,
                                             .data$conf.low,
                                             .data$conf.high),
                         .funs = exp) },
    # survdiff_ci() already returns tidy-ish data:
    survdiff =,
    survdiff_joint = ,
    cumincdiff = ,
    cumincdiff_joint = {
      fit %>% dplyr::mutate(term = paste0(".exposure", .data$term))
    },
    irrrob =,
    irrrob_joint = {
      if (!requireNamespace("sandwich", quietly = TRUE)) {
        stop(paste(
          "The package \"sandwich\" must be installed to estimate robust",
          "standard errors.\nInstall with:  install.packages(\"sandwich\")"),
          call. = FALSE)
      }
      broom::tidy(fit,
                  conf.int = FALSE,
                  exponentiate = FALSE) %>%
        dplyr::mutate(
          std.error = sqrt(diag(sandwich::vcovHC(fit,
                                                 type = "HC0"))),
          statistic = .data$estimate / .data$std.error,
          conf.low = .data$estimate -
            stats::qnorm(0.975) * .data$std.error,
          conf.high = .data$estimate +
            stats::qnorm(0.975) * .data$std.error) %>%
        dplyr::mutate_at(
          .vars = dplyr::vars(.data$estimate,
                              .data$conf.low,
                              .data$conf.high),
          .funs = exp)
    },
    # standard- otherwise:
    broom::tidy(fit, conf.int = TRUE,
                exponentiate = exponent,
                # only used for rr or rd with margstd:
                bootrepeats = bootrepeats))
  fit <- fit %>%
    dplyr::select(.data$term, .data$estimate,
                  .data$conf.low, .data$conf.high) %>%
    dplyr::mutate(
      ref_rrrd = (.data$conf.low == .data$conf.high &
                    .data$conf.low == .data$estimate &
                    estimand %in% c("rr", "rd", "rd_joint", "rr_joint")),
      nonconverg = (.data$conf.low == 0 &
                      .data$conf.high == Inf)) %>%
    dplyr::mutate_at(.vars = dplyr::vars(.data$estimate,
                                         .data$conf.low,
                                         .data$conf.high),
                     .funs = ~format(round(. * multiply, digits = digits),
                                     nsmall = digits,
                                     trim = TRUE, scientific = FALSE)) %>%
    dplyr::full_join(
      count_per_stratum %>%
        dplyr::mutate(
          .exposure = paste0(
            ".exposure",
            .data$.exposure)),
      by = c(term = ".exposure")) %>%
    dplyr::filter(stringr::str_detect(string = .data$term,
                                      pattern = pattern)) %>%
    dplyr::mutate(.exposure = stringr::str_remove(string = .data$term,
                                                  pattern = pattern))
  if(is_trend == TRUE) {
    fit <- fit %>%
      dplyr::slice(1) %>%
      dplyr::mutate(.exposure = "Trend")
  } else {
    fit <- fit %>%
      dplyr::left_join(x = tibble::tibble(.exposure = xlevels),
                       by = ".exposure")
  }

  fit %>%
    dplyr::mutate(
      res = dplyr::case_when(
        .data$nonconverg == TRUE |
          is.na(.data$conf.low) |
          as.character(.data$conf.low) == "NA" ~
          "--",
        TRUE ~
          paste0(.data$estimate, " (",
                 .data$conf.low, to,
                 .data$conf.high, ")")),
      res = dplyr::if_else(
        (is.na(.data$estimate) | .data$ref_rrrd == TRUE) &
          dplyr::row_number() == 1,
        true = paste(reference, "(reference)"),
        false = .data$res),
      res = dplyr::if_else(.data$.per_stratum < nmin,
                           true = "--",
                           false = .data$res)) %>%
    dplyr::select(.data$.exposure, .data$res)
}
