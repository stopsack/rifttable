#' Estimate Difference in Survival or Cumulative Incidence and Confidence Interval
#'
#' @description
#' This function estimates the unadjusted difference in survival at a given time
#' point based on the difference between per-group Kaplan-Meier estimates.
#'
#' @param formula Formula of a survival object using
#'   \code{\link[survival]{Surv}} of the form, \code{Surv(time, event) ~ group}.
#'   The exposure variable (here, \code{group}) must be categorical with at
#'   least 2 categories.
#' @param data Data set.
#' @param time Time point to estimate survival difference at.
#' @param estimand Optional. Estimate difference in survival (\code{"survival"})
#'   or cumulative incidence (\code{"cuminc"})? This parameter affects the
#'   sign of the differences. Defaults to \code{"survival"}.
#' @param conf.level Optional. Confidence level. Defaults to \code{0.95}.
#'
#' @references
#' Com-Nougue C, Rodary C, Patte C. How to establish equivalence when data are
#' censored: a randomized trial of treatments for B non-Hodgkin lymphoma.
#' Stat Med 1993;12:1353–64. \url{https://doi.org/10.1002/sim.4780121407}.
#'
#' Altman DG, Andersen PK. Calculating the number needed to treat for trials
#' where the outcome is time to an event. BMJ 1999;319:1492–5.
#' \url{https://doi.org/10.1136/bmj.319.7223.14929}.
#'
#' @return
#' Tibble in \code{\link[broom]{tidy}} format:
#'
#' * \code{term} Name of the exposure stratum.
#' * \code{estimate} Difference in survival at \code{time}.
#' * \code{std.error} Large-sample standard error of the difference in survival
#'    functions (see References). For each survival function, Greenwood
#'    standard errors with log transformation are used, the default of the
#'    survival package/\code{\link[survival]{survfit}}).
#' * \code{statistic} z statistic.
#' * \code{p.value}
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
#'   dplyr::mutate(sex = factor(sex, levels = 1:2,
#'                              labels = c("Men", "Women")),
#'                 status = status - 1)
#'
#' survdiff_ci(formula = survival::Surv(time = time, event = status) ~ sex,
#'             data = cancer,
#'             time = 365.25)
#' # Women have 19 percentage points higher one-year survival than men
#' # (95% CI, 5 to 34 percentage points).
survdiff_ci <- function(formula, data, time,
                        estimand = c("survival", "cuminc"),
                        conf.level = 0.95) {
  zval <- stats::qnorm(1 - (1 - conf.level) / 2)
  estimand <- match.arg(estimand)
  res <- summary(survival::survfit(formula = formula, data = data), time = time)
  res <- tibble::tibble(term = res$strata,
                        surv = res$surv,
                        se = res$std.err)
  if(estimand == "cuminc")
    res$surv <- 1 - res$surv
  res %>%
    dplyr::transmute(
      term = stringr::str_remove_all(string = .data$term,
                                     pattern = "([:alnum:]|\\.|_)+="),
      estimate = .data$surv - .data$surv[1],
      std.error = sqrt(.data$se^2 + .data$se[1]^2),
      statistic = .data$estimate / .data$std.error,
      p.value = 1 - stats::pnorm(.data$statistic),
      conf.low = .data$estimate - zval * .data$std.error,
      conf.high = .data$estimate + zval * .data$std.error) %>%
    dplyr::slice(-1)
}
