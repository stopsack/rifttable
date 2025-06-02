#' Estimate Difference in Survival or Cumulative Incidence and Confidence Interval
#'
#' @description
#' This function estimates the unadjusted difference or ratio in survival or
#' cumulative incidence (risk) at a given time point based on the difference
#' between per-group Kaplan-Meier estimates or, if competing events are present,
#' Aalen-Johansen estimates of the cumulative incidence.
#'
#' For constructing confidence limits, the MOVER approach described by Zou and
#' Donner (2008) is used, with estimation on the log scale for ratios.
#'
#' @param formula Formula of a survival object using
#'   \code{\link[survival]{Surv}} of the form, \code{Surv(time, event) ~ group}.
#'   The exposure variable (here, \code{group}) must be categorical with at
#'   least 2 categories.
#' @param data Data set.
#' @param time Time point to estimate survival difference at.
#' @param estimand Optional. Estimate difference in survival (\code{"survival"})
#'   or cumulative incidence (\code{"cuminc"})? This parameter affects the
#'   sign of the differences. Only \code{"cuminc"} is available if competing
#'   events are present, i.e., \code{event_type} is not \code{NULL}.
#'   Defaults to \code{"survival"}.
#' @param type Optional. Estimate differences (\code{"diff"}) or ratio
#'   (\code{"ratio"}) of survival or cumulative incidence? Defaults to
#'   \code{"diff"}.
#' @param approach Optional. For estimating confidence limits of differences,
#'   use the MOVER approach based on upper and lower confidence limits of each
#'   group (\code{"mover"}), or square-and-add standard errors
#'   (\code{"squareadd"})? Defaults to \code{"mover"}. (For confidence limits of
#'   ratios, this argument is ignored and MOVER is used.)
#' @param conf.level Optional. Confidence level. Defaults to \code{0.95}.
#' @param event_type Optional. Event type (level) for event variable with
#'   competing events. Defaults to \code{NULL}.
#' @param id_variable Optional. Identifiers for individual oberversations,
#'   required if data are clustered, or if competing events and time/time2
#'   notation are used concomitantly.
#' @param weighted Optional. Weigh survival curves, e.g. for inverse-probability
#'   weighting, before estimating differences or ratios? If \code{TRUE}, the
#'   \code{data} must contain a variable called \code{.weights}. Defaults to
#'   \code{FALSE}.
#'
#' @references
#' Com-Nougue C, Rodary C, Patte C. How to establish equivalence when data are
#' censored: a randomized trial of treatments for B non-Hodgkin lymphoma.
#' Stat Med 1993;12:1353–64. \doi{10.1002/sim.4780121407}.
#'
#' Altman DG, Andersen PK. Calculating the number needed to treat for trials
#' where the outcome is time to an event. BMJ 1999;319:1492–5.
#' \doi{10.1136/bmj.319.7223.1492}.
#'
#' Zou GY, Donner A. Construction of confidence limits about effect measures:
#' A general approach. Statist Med 2008;27:1693–1702.
#' \doi{10.1002/sim.3095}
#'
#' @return
#' Tibble in \code{\link[broom]{tidy}} format:
#'
#' * \code{term} Name of the exposure stratum.
#' * \code{estimate} Difference or ratio.
#' * \code{std.error} Large-sample standard error of the difference in survival
#'    functions (see References). For each survival function, Greenwood
#'    standard errors with log transformation are used, the default of the
#'    survival package/\code{\link[survival]{survfit}}).
#' * \code{statistic} z statistic.
#' * \code{p.value} From the z statistic.
#' * \code{conf.low} Lower confidence limit
#' * \code{conf.high} Upper confidence limit
#'
#' @export
#'
#' @examples
#' # Load 'cancer' dataset from survival package (Used in all examples)
#' data(cancer, package = "survival")
#'
#' cancer <- cancer |>
#'   dplyr::mutate(
#'     sex = factor(
#'       sex,
#'       levels = 1:2,
#'       labels = c("Male", "Female")
#'     ),
#'     status = status - 1
#'   )
#'
#' survdiff_ci(
#'   formula = survival::Surv(time = time, event = status) ~ sex,
#'   data = cancer,
#'   time = 365.25
#' )
#' # Females have 19 percentage points higher one-year survival than males
#' # (95% CI, 5 to 34 percentage points).
survdiff_ci <- function(
    formula,
    data,
    time,
    estimand = c("survival", "cuminc"),
    type = c("diff", "ratio"),
    approach = c("mover", "squareadd"),
    conf.level = 0.95,
    event_type = NULL,
    id_variable = NULL,
    weighted = FALSE) {
  .id <- NULL # address seemingly global variable ".id" in survfit() call
  .weights <- NULL # same
  # If this function is called outside rifttable(), which would create .id
  if(!(".id" %in% names(data))) {
    data$.id <- find_id(
      data = data,
      id_variable = id_variable
    )
  }
  if (weighted == FALSE) {
    data$.weights <- 1
  }
  zval <- stats::qnorm(1 - (1 - conf.level) / 2)
  estimand <- match.arg(estimand)
  type <- match.arg(type)
  approach <- match.arg(approach)
  res <- summary(
    survival::survfit(
      formula = formula,
      data = data,
      id = .id,
      weights = .weights
    ),
    time = time,
    extend = TRUE
  )
  if (is.null(event_type)) {
    res <- tibble::tibble(
      term = res$strata,
      surv = res$surv,
      se = res$std.err,
      lci = res$lower,
      uci = res$upper
    )
    if (estimand == "cuminc") {
      res <- res |>
        dplyr::mutate(
          surv = 1 - .data$surv,
          lci = 1 - .data$lci,
          uci = 1 - .data$uci
        )
    }
  } else {
    if (estimand == "survival") {
      stop(paste(
        "type = 'survdiff' or 'survratio' may not be meaningful with",
        "competing events. Use: type = 'cumincdiff' or 'cumincratio'."
      ))
    }
    res <- tibble::tibble(
      term = res$strata,
      surv = res$pstate[, which(res$states == event_type)],
      se = res$std.err[, which(res$states == event_type)],
      lci = res$lower[, which(res$states == event_type)],
      uci = res$upper[, which(res$states == event_type)]
    )
  }
  if (type == "diff" & approach == "squareadd") {
    res <- res |>
      dplyr::transmute(
        term = stringr::str_remove_all(
          string = .data$term,
          pattern = "([:alnum:]|\\.|_)+="
        ),
        estimate = .data$surv - .data$surv[1],
        std.error = sqrt(.data$se^2 + .data$se[1]^2),
        statistic = .data$estimate / .data$std.error,
        p.value = 1 - stats::pnorm(abs(.data$statistic)),
        conf.low = .data$estimate - zval * .data$std.error,
        conf.high = .data$estimate + zval * .data$std.error
      ) |>
      dplyr::slice(-1)
  }
  if (type == "diff" & approach == "mover") {
    res <- res |>
      dplyr::transmute(
        term = stringr::str_remove_all(
          string = .data$term,
          pattern = "([:alnum:]|\\.|_)+="
        ),
        estimate = .data$surv - .data$surv[1],
        conf.low = .data$estimate -
          sqrt(
            (.data$surv - .data$lci)^2 +
              (.data$uci[1] - .data$surv[1])^2
          ),
        conf.high = .data$estimate +
          sqrt(
            (.data$uci - .data$surv)^2 +
              (.data$surv[1] - .data$lci[1])^2
          ),
        std.error = (.data$conf.high - .data$conf.low) / 2 / zval,
        statistic = .data$estimate / .data$std.error,
        p.value = 1 - stats::pnorm(abs(.data$statistic))
      ) |>
      dplyr::select(
        "term", "estimate", "std.error", "statistic", "p.value",
        "conf.low", "conf.high"
      ) |>
      dplyr::slice(-1)
  }
  if (type == "ratio") {
    res <- res |>
      dplyr::mutate(
        surv = log(.data$surv),
        lci = log(.data$lci),
        uci = log(.data$uci),
      ) |>
      dplyr::transmute(
        term = stringr::str_remove_all(
          string = .data$term,
          pattern = "([:alnum:]|\\.|_)+="
        ),
        estimate = .data$surv - .data$surv[1],
        conf.low = exp(
          .data$estimate -
            sqrt(
              (.data$surv - .data$lci)^2 +
                (.data$uci[1] - .data$surv[1])^2
            )
        ),
        conf.high = exp(
          .data$estimate +
            sqrt(
              (.data$uci - .data$surv)^2 +
                (.data$surv[1] - .data$lci[1])^2
            )
        ),
        std.error = (log(.data$conf.high) - log(.data$conf.low)) / 2 / zval,
        statistic = .data$estimate / .data$std.error,
        p.value = 1 - stats::pnorm(abs(.data$statistic)),
        estimate = exp(.data$estimate)
      ) |>
      dplyr::select(
        "term", "estimate", "std.error", "statistic", "p.value",
        "conf.low", "conf.high"
      ) |>
      dplyr::slice(-1)
  }
  return(res)
}
