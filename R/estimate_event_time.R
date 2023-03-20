estimate_event_time <- function(
    data,
    type,
    time2,
    digits,
    nmin,
    na_rm,
    ci,
    diff_digits,
    risk_digits,
    rate_digits,
    risk_percent,
    factor,
    to,
    arguments,
    is_trend,
    ...) {
  if(is_trend)
    return(tibble::tibble())
  digits <- find_rounding_digits(
    digits = digits,
    default = switch(
      EXPR = type,
      "medsurv" =,
      "medsurv (ci)" =,
      "medfu" =,
      "medfu (iqr)" =,
      "maxfu" = diff_digits,

      "cuminc" =,
      "cuminc (ci)" =,
      "surv" =,
      "surv (ci)" = risk_digits,

      "rate" =,
      "rate (ci)" =,
      "events/time (rate)" = rate_digits,

      "time" =,
      "events/time" = 0,
      4))
  data <- data %>%
    dplyr::group_by(
      .data$.exposure,
      .drop = FALSE)
  percent_symbol <- dplyr::if_else(
    risk_percent == TRUE,
    true = "%",
    false = "")
  percent_100 <- dplyr::if_else(
    risk_percent == TRUE,
    true = 100,
    false = 1)
  timepoint <- find_argument(
    arguments = arguments,
    which_argument = "timepoint",
    is_numeric = TRUE)  # default: NA

  switch(
    EXPR = type,
    "events" = {
      data %>%
        dplyr::summarize(res = paste(sum(.data$.event)))
    },
    "time" = {
      data %>%
        dplyr::summarize(res = paste(
          format_round(
            sum(.data$.time),
            digits = digits)))
    },
    "events/time" = {
      data %>%
        dplyr::summarize(res = paste(
          sum(.data$.event),
          format_round(
            sum(.data$.time),
            digits = digits),
          sep = "/"))
    },
    "events/total" = {
      data %>%
        dplyr::summarize(res = paste(
          sum(.data$.event),
          dplyr::n(),
          sep = "/"))
    },
    "rate" = {
      data %>%
        dplyr::summarize(res = format_round(
          sum(.data$.event) *
            factor /
            sum(.data$.time),
          digits = digits))
    },
    "rate (ci)" = {
      data %>%
        dplyr::summarize(res = paste0(
          format_round(
            sum(.data$.event) *
              factor /
              sum(.data$.time),
            digits = digits),
          " (",
          format_round(
            factor *
              exp(log(
                sum(.data$.event) /
                  sum(.data$.time))
                - stats::qnorm(1 - (1 - ci) / 2) *
                  1/sqrt(sum(.data$.event))),
            digits = digits),
          to,
          format_round(
            factor *
              exp(log(sum(.data$.event) /
                        sum(.data$.time))
                  + stats::qnorm(1 - (1 - ci) / 2) *
                    1/sqrt(sum(.data$.event))),
            digits = digits),
          ")"))
    },
    "events/time (rate)" = {
      data %>%
        dplyr::summarize(res = paste0(
          sum(.data$.event),
          "/",
          format_round(
            sum(.data$.time),
            digits = 0),
          " (",
          format_round(
            sum(.data$.event) *
              factor /
              sum(.data$.time),
            digits = digits),
          ")"))
    },
    "medsurv" =,
    "medsurv (ci)" = {
      data %>%
        dplyr::summarize(
          res = {
            if(is.na(time2[1])) {
              fit <- survival::survfit(
                formula = survival::Surv(
                  time  = .data$.time,
                  event = .data$.event) ~ 1,
                conf.int = ci)
            } else {
              fit <- survival::survfit(
                formula = survival::Surv(
                  time  = .data$.time_orig,
                  time2 = .data$.time2,
                  event = .data$.event) ~ 1,
                conf.int = ci)
            }
            paste0(
              format_round(
                summary(fit)$table["median"],
                digits = digits),
              dplyr::if_else(stringr::str_detect(
                string = type,
                pattern = "(ci)"),
                true = paste0(
                  " (",
                  format_round(
                    stats::quantile(fit, probs = 0.5)$lower[[1]],
                    digits = digits),
                  to,
                  format_round(
                    stats::quantile(fit, probs = 0.5)$upper[[1]],
                    digits = digits),
                  ")"),
                false = ""))
          })
    },
    "medfu" =,
    "medfu (iqr)" = {
      data %>%
        dplyr::summarize(
          res = {
            if(is.na(time2[1])) {
              fit <- survival::survfit(
                survival::Surv(
                  time  = .data$.time,
                  event = !.data$.event) ~ 1)
            } else {
              fit <- survival::survfit(
                survival::Surv(
                  time  = .data$.time_orig,
                  time2 = .data$.time2,
                  event = !.data$.event) ~ 1)
            }
            paste0(
              format_round(
                stats::quantile(fit, probs = 0.5)$quantile[[1]],
                digits = digits),
              dplyr::if_else(stringr::str_detect(
                string = type,
                pattern = "(iqr)"),
                true = paste0(
                  " (",
                  format_round(
                    stats::quantile(fit, probs = 0.25)$quantile[[1]],
                    digits = digits),
                  to,
                  format_round(
                    stats::quantile(fit, probs = 0.75)$quantile[[1]],
                    digits = digits),
                  ")"),
                false = ""))
          })
    },
    "maxfu" = {
      data %>%
        dplyr::summarize(
          res = paste(format_round(
            max(.data$.time),
            digits = digits)))
    },
    "surv" =,
    "surv (ci)" =,
    "cuminc" =,
    "cuminc (ci)" = {
      data %>%
        dplyr::summarize(
          res = {
            if(is.na(time2[1])) {
              fit <- survival::survfit(
                formula = survival::Surv(
                  time  = .data$.time,
                  event = .data$.event) ~ 1,
                conf.int = ci)
            } else {
              fit <- survival::survfit(
                formula = survival::Surv(
                  time  = .data$.time_orig,
                  time2 = .data$.time2,
                  event = .data$.event) ~ 1,
                conf.int = ci)
            }
            if(is.na(timepoint))
              fit <- summary(fit)
            else
              fit <- summary(
                fit,
                times = timepoint,
                extend = TRUE)  # estimate even if all censored

            if(stringr::str_detect(
              string = type,
              pattern = "cuminc")) {
              added <- 1
              multiply <- -1
              first_limit <- "upper"
              second_limit <- "lower"
            } else {
              added <- 0
              multiply <- 1
              first_limit <- "lower"
              second_limit <- "upper"
            }

            paste0(
              format_round(
                (added +
                   multiply *
                   (fit %>%
                      purrr::pluck("surv") %>%
                      dplyr::last())) *
                  percent_100,
                digits = digits),
              percent_symbol,
              dplyr::if_else(stringr::str_detect(
                string = type,
                pattern = "(ci)"),
                true = paste0(
                  " (",
                  format_round(
                    (added +
                       multiply *
                       (fit %>%
                          purrr::pluck(first_limit) %>%
                          dplyr::last())) *
                      percent_100,
                    digits = digits),
                  to,
                  format_round(
                    (added +
                       multiply *
                       (fit %>%
                          purrr::pluck(second_limit) %>%
                          dplyr::last())) *
                      percent_100,
                    digits = digits),
                  ")"),
                false = ""))
          })
    }) %>%
    format_stratified_results(
      data = data,
      to = to,
      nmin = nmin,
      suppress = "event",
      is_trend = is_trend)
}
