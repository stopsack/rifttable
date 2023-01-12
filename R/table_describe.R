#' Get counts
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param time2 Optional second time variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param type Type of statistic quested
#' @param factor Factor for rates. Defaults to 1000.
#' @param risk_percent Display risks as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator for mean/difference confidence intervals
#'
#' @return Tibble
#' @noRd
table_counts <- function(data, event, time, time2, outcome,
                         effectmodifier = NULL, effectmodifier_level = NULL,
                         type, factor, risk_percent,
                         digits, to, nmin) {
  if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
    if(!is.null(effectmodifier_level) & !is.null(effectmodifier)) {
      if(!is.na(effectmodifier)) {
        data <- data %>%
          dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
          dplyr::filter(.data$.effectmod %in% effectmodifier_level)
      }
    }
  }

  if(stringr::str_detect(
    string = type,
    pattern = "outcomes|risk|mean|median|cases|range|^sd")) {
    data <- data %>%
      dplyr::select(.data$.exposure, outcome = dplyr::one_of(outcome)) %>%
      dplyr::mutate(event = NA, time = NA)
  } else {
    if(stringr::str_detect(
      string = type,
      pattern = "events|rate|medsurv|medfu|maxfu|cuminc|^surv") |
      type == "time") {
      # with 'time' and 'time2', the first is enter and the second is exit
      if(!is.na(time2)) {
        data <- data %>%
          dplyr::select(.data$.exposure,
                        event      = dplyr::one_of(event),
                        time_orig  = dplyr::one_of(time),
                        time2      = dplyr::one_of(time2)) %>%
          # for estimators that just sum the follow-up times:
          dplyr::mutate(time = .data$time2 - .data$time_orig,
                        outcome = NA)
      } else {
        data <- data %>% dplyr::select(.data$.exposure,
                                       event     = dplyr::one_of(event),
                                       time      = dplyr::one_of(time)) %>%
          dplyr::mutate(outcome = NA)
      }
    } else {
      data <- data %>%
        dplyr::select(.data$.exposure) %>%
        dplyr::mutate(event = NA, time = NA, outcome = NA)
    }
  }
  if(is.null(to))
    to <- dplyr::if_else(stringr::str_detect(string = type,
                                             pattern = "mean|median|range"),
                         true = " to ", false = "-")

  # Extract specific follow-up time for cumulative incidence/survival,
  # if provided:
  if(stringr::str_detect(string = type, pattern = "cuminc|^surv")) {
    timepoint <- stringr::str_remove_all(string = type,
                                         pattern = "cuminc|^surv|\\(ci\\)|\\h")
    type_new <- dplyr::if_else(stringr::str_detect(string = type,
                                                   pattern = "cuminc"),
                               true = "cuminc",
                               false = "surv")
    type <- dplyr::if_else(stringr::str_detect(string = type,
                                               pattern = "\\(ci\\)"),
                           true = paste(type_new, "(ci)"),
                           false = type_new)
    if(stringr::str_length(string = timepoint) > 0) {
      if(is.na(as.numeric(timepoint)))
        stop(paste0("The supplied time for cumulative incidence is not ",
                    "a valid numeric value: '",
                    timepoint, "'."))
      timepoint <- as.numeric(timepoint)
    } else {
      timepoint <- NA_real_
    }
  }
  if(is.na(nmin))
    nmin <- 0
  count_per_stratum <- counts_per_stratum(
    data = data,
    event = event,
    estimand = type,
    is_trend = FALSE)

  data %>%
    dplyr::group_by(.data$.exposure,
                    .drop = FALSE) %>%
    dplyr::summarize(res = dplyr::case_when(
      type == "outcomes"              ~ paste(sum(.data$outcome)),
      type == "events"                ~ paste(sum(.data$event)),
      type == "time"                  ~
        paste(trimws(format(round(sum(.data$time), digits = digits),
                            nsmall = digits))),
      type == "total"                 ~ paste(n()),
      type == "outcomes/total"        ~ paste(sum(.data$outcome), n(),
                                              sep = "/"),
      type == "events/time"           ~
        paste(sum(.data$event),
              trimws(format(round(sum(.data$time), digits = digits),
                            nsmall = digits)),
              sep = "/"),
      type == "events/total"          ~ paste(sum(.data$event), n(),
                                              sep = "/"),
      type == "cases/controls"        ~ paste(sum(.data$outcome),
                                              sum(!.data$outcome),
                                              sep = "/"),
      type == "risk"                  ~
        paste0(trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = "")),
      type == "risk (ci)"             ~
        paste0(trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), " (",
               trimws(format(round(scoreci(success = sum(.data$outcome),
                                           total = n())$conf.low *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)), to,
               trimws(format(round(scoreci(success = sum(.data$outcome),
                                           total = n())$conf.high *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)), ")"),
      type == "rate"                  ~
        trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                            digits = digits), nsmall = digits)),
      type == "rate (ci)"             ~
        paste0(trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                                   digits = digits), nsmall = digits)), " (",
               trimws(format(round(factor * exp(log(sum(.data$event) /
                                                      sum(.data$time))
                                                - stats::qnorm(0.975) *
                                                  1/sqrt(sum(.data$event))),
                                   digits = digits), nsmall = digits)),
               to,
               trimws(format(round(factor * exp(log(sum(.data$event) /
                                                      sum(.data$time))
                                                + stats::qnorm(0.975) *
                                                  1/sqrt(sum(.data$event))),
                                   digits = digits), nsmall = digits)), ")"),
      type == "outcomes (risk)" ~
        paste0(sum(.data$outcome), " (",
               trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), ")"),
      type == "outcomes/total (risk)" ~
        paste0(sum(.data$outcome), "/", n(), " (",
               trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), ")"),
      type == "events/time (rate)"    ~
        paste0(sum(.data$event), "/",
               trimws(format(round(sum(.data$time), digits = 0), nsmall = 0)),
               " (",
               trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                                   digits = digits), nsmall = digits)), ")"),
      type == "medsurv" ~
        if(type == "medsurv") {
          if(is.na(time2[1])) {
            fit <- survival::survfit(survival::Surv(time  = .data$time,
                                                    event = .data$event) ~ 1)
          } else {
            fit <- survival::survfit(survival::Surv(time  = .data$time_orig,
                                                    time2 = .data$time2,
                                                    event = .data$event) ~ 1)
          }
          trimws(format(round(
            summary(fit)$table["median"],
            digits = digits), nsmall = digits))
        } else { "" },
      type == "medsurv (ci)" ~
        if(type == "medsurv (ci)") {
          if(is.na(time2[1])) {
            fit <- survival::survfit(survival::Surv(time  = .data$time,
                                                    event = .data$event) ~ 1)
          } else {
            fit <- survival::survfit(survival::Surv(time  = .data$time_orig,
                                                    time2 = .data$time2,
                                                    event = .data$event) ~ 1)
          }
          paste0(trimws(format(round(quantile(fit, probs = 0.5)$quantile[[1]],
                                     digits = digits),
                               nsmall = digits)), " (",
                 trimws(format(round(quantile(fit, probs = 0.5)$lower[[1]],
                                     digits = digits),
                               nsmall = digits)),
                 to,
                 trimws(format(round(quantile(fit, probs = 0.5)$upper[[1]],
                                     digits = digits),
                               nsmall = digits)), ")")
        } else { "" },
      type == "medfu" ~
        if(type == "medfu") {
          if(is.na(time2[1])) {
            fit <- survival::survfit(survival::Surv(time  = .data$time,
                                                    event = !.data$event) ~ 1)
          } else {
            fit <- survival::survfit(survival::Surv(time  = .data$time_orig,
                                                    time2 = .data$time2,
                                                    event = !.data$event) ~ 1)
          }
          trimws(format(round(
            summary(fit)$table["median"],
            digits = digits), nsmall = digits))
        } else { "" },
      type == "medfu (iqr)" ~
        if(type == "medfu (iqr)") {
          if(is.na(time2[1])) {
            fit <- survival::survfit(survival::Surv(time  = .data$time,
                                                    event = !.data$event) ~ 1)
          } else {
            fit <- survival::survfit(survival::Surv(time  = .data$time_orig,
                                                    time2 = .data$time2,
                                                    event = !.data$event) ~ 1)
          }
          paste0(trimws(format(round(quantile(fit, probs = 0.5)$quantile[[1]],
                                     digits = digits),
                               nsmall = digits)), " (",
                 trimws(format(round(quantile(fit, probs = 0.25)$quantile[[1]],
                                     digits = digits),
                               nsmall = digits)),
                 to,
                 trimws(format(round(quantile(fit, probs = 0.75)$quantile[[1]],
                                     digits = digits),
                               nsmall = digits)), ")")
        } else { "" },
      type == "maxfu" ~
        if(type == "maxfu") {
          paste(trimws(format(round(max(.data$time), digits = 0),
                              nsmall = 0)))
        } else { "" },
      type == "cuminc" ~
        if(type == "cuminc") {
          if(is.na(time2[1])) {
            fit <- survival::survfit(survival::Surv(time  = .data$time,
                                                    event = .data$event) ~ 1)
          } else {
            fit <- survival::survfit(survival::Surv(time  = .data$time_orig,
                                                    time2 = .data$time2,
                                                    event = .data$event) ~ 1)
          }
          if(is.na(timepoint)) {
            paste0(trimws(format(round(
              (1 - (summary(fit) %>%
                      purrr::pluck("surv") %>%
                      dplyr::last())) *
                if_else(risk_percent == TRUE,
                        true = 100, false = 1),
              digits = digits), nsmall = digits)),
              if_else(risk_percent == TRUE, true = "%", false = ""))
          } else {
            paste0(trimws(format(round(
              (1 - (summary(fit, times = timepoint) %>%
                      purrr::pluck("surv"))) *
                if_else(risk_percent == TRUE,
                        true = 100, false = 1),
              digits = digits), nsmall = digits)),
              if_else(risk_percent == TRUE, true = "%", false = ""))
          }
        } else { "" },
      type == "cuminc (ci)" ~
        if(type == "cuminc (ci)") {
          if(is.na(time2[1])) {
            fit <- survival::survfit(survival::Surv(time  = .data$time,
                                                    event = .data$event) ~ 1)
          } else {
            fit <- survival::survfit(survival::Surv(time  = .data$time_orig,
                                                    time2 = .data$time2,
                                                    event = .data$event) ~ 1)
          }

          if(is.na(timepoint)) {
            fit <- summary(fit)
          } else {
            fit <- summary(fit, times = timepoint)
          }

          paste0(trimws(format(round((1 - (fit %>% purrr::pluck("surv") %>%
                                             dplyr::last())) *
                                       if_else(risk_percent == TRUE,
                                               true = 100, false = 1),
                                     digits = digits),
                               nsmall = digits)),
                 if_else(risk_percent == TRUE, true = "%", false = ""), " (",
                 trimws(format(round((1 - (fit %>% purrr::pluck("upper") %>%
                                             dplyr::last())) *
                                       if_else(risk_percent == TRUE,
                                               true = 100, false = 1),
                                     digits = digits),
                               nsmall = digits)),
                 to,
                 trimws(format(round((1 - (fit %>% purrr::pluck("lower") %>%
                                             dplyr::last())) *
                                       if_else(risk_percent == TRUE,
                                               true = 100, false = 1),
                                     digits = digits),
                               nsmall = digits)), ")")
        } else { "" },
      type == "surv" ~
        if(type == "surv") {
          if(is.na(time2[1])) {
            fit <- survival::survfit(survival::Surv(time  = .data$time,
                                                    event = .data$event) ~ 1)
          } else {
            fit <- survival::survfit(survival::Surv(time  = .data$time_orig,
                                                    time2 = .data$time2,
                                                    event = .data$event) ~ 1)
          }
          if(is.na(timepoint)) {
            paste0(trimws(format(round(
              summary(fit) %>%
                purrr::pluck("surv") %>%
                dplyr::last() *
                if_else(risk_percent == TRUE,
                        true = 100, false = 1),
              digits = digits), nsmall = digits)),
              if_else(risk_percent == TRUE, true = "%", false = ""))
          } else {
            paste0(trimws(format(round(
              summary(fit, times = timepoint) %>%
                purrr::pluck("surv") *
                if_else(risk_percent == TRUE,
                        true = 100, false = 1),
              digits = digits), nsmall = digits)),
              if_else(risk_percent == TRUE, true = "%", false = ""))
          }
        } else { "" },
      type == "surv (ci)" ~
        if(type == "surv (ci)") {
          if(is.na(time2[1])) {
            fit <- survival::survfit(survival::Surv(time  = .data$time,
                                                    event = .data$event) ~ 1)
          } else {
            fit <- survival::survfit(survival::Surv(time  = .data$time_orig,
                                                    time2 = .data$time2,
                                                    event = .data$event) ~ 1)
          }

          if(is.na(timepoint)) {
            fit <- summary(fit)
          } else {
            fit <- summary(fit, times = timepoint)
          }

          paste0(trimws(format(round(fit %>% purrr::pluck("surv") %>%
                                       dplyr::last() *
                                       if_else(risk_percent == TRUE,
                                               true = 100, false = 1),
                                     digits = digits),
                               nsmall = digits)),
                 if_else(risk_percent == TRUE, true = "%", false = ""), " (",
                 trimws(format(round(fit %>% purrr::pluck("lower") %>%
                                       dplyr::last() *
                                       if_else(risk_percent == TRUE,
                                               true = 100, false = 1),
                                     digits = digits),
                               nsmall = digits)),
                 to,
                 trimws(format(round(fit %>% purrr::pluck("upper") %>%
                                       dplyr::last() *
                                       if_else(risk_percent == TRUE,
                                               true = 100, false = 1),
                                     digits = digits),
                               nsmall = digits)), ")")
        } else { "" },
      type == "mean" ~
        trimws(format(round(mean(.data$outcome), digits = digits),
                      nsmall = digits)),
      type == "mean (sd)" ~
        paste0(trimws(format(round(mean(.data$outcome), digits = digits),
                             nsmall = digits)), " (",
               trimws(format(round(sd(.data$outcome), digits = digits),
                             nsmall = digits)), ")"),
      type == "sd" ~
        trimws(format(round(sd(.data$outcome), digits = digits),
                      nsmall = digits)),
      type == "mean (ci)" ~
        paste0(trimws(format(round(mean(.data$outcome), digits = digits),
                             nsmall = digits)), " (",
               trimws(format(round(mean(.data$outcome) - stats::qnorm(0.975) *
                                     sqrt(var(.data$outcome) /
                                            sum(!is.na(.data$outcome))),
                                   digits = digits),
                             nsmall = digits)),
               to,
               trimws(format(round(mean(.data$outcome) + stats::qnorm(0.975) *
                                     sqrt(var(.data$outcome) /
                                            sum(!is.na(.data$outcome))),
                                   digits = digits),
                             nsmall = digits)), ")"),
      type == "median" ~
        trimws(format(round(median(.data$outcome), digits = digits),
                      nsmall = digits)),
      type == "median (iqr)" ~
        if(type == "median (iqr)") {
          paste0(trimws(format(round(median(.data$outcome),
                                     digits = digits), nsmall = digits)),
                 " (",
                 trimws(format(round(stats::quantile(.data$outcome,
                                                     probs = 0.25),
                                     digits = digits), nsmall = digits)),
                 to,
                 trimws(format(round(stats::quantile(.data$outcome,
                                                     probs = 0.75),
                                     digits = digits), nsmall = digits)), ")")
        } else { "" },
      type == "range" ~
        if(type == "range") {
          if(sum(!is.na(.data$outcome)) > 0) {
            paste0(trimws(format(round(min(.data$outcome),
                                       digits = digits), nsmall = digits)),
                   to,
                   trimws(format(round(max(.data$outcome),
                                       digits = digits), nsmall = digits)))
          } else { "--" }
        } else { "" }),
      .groups = "drop") %>%
    dplyr::left_join(count_per_stratum,
                     by = ".exposure") %>%
    dplyr::mutate(
      res = dplyr::if_else(
        stringr::str_remove(string = .data$res,
                            pattern = "%") %in%
          c("NaN", "NA", "NaN (NA)",
            paste0("NA (NA", to, "NA)"),
            paste0("NaN (NaN", to, "NaN)")),
        true = "--", false = .data$res),
      res = dplyr::case_when(
        stringr::str_detect(string = .data$res,
                            pattern = stringr::fixed("(NaN)")) ~
          paste0(stringr::str_remove(string = .data$res,
                                     pattern = stringr::fixed("(NaN)")),
                 "(--)"),
        stringr::str_detect(string = .data$res,
                            pattern = stringr::fixed("(NaN%)")) ~
          paste0(stringr::str_remove(string = .data$res,
                                     pattern = stringr::fixed("(NaN%)")),
                 "(--)"),
        TRUE ~ .data$res),
      res = dplyr::if_else(.data$.per_stratum < nmin,
                           true = "--",
                           false = .data$res)) %>%
    dplyr::select(-.data$.per_stratum)
}
