#' Wilson Score Confidence Intervals
#'
#' @description
#' "This function computes a confidence interval for a proportion.
#' It is based on inverting the large-sample normal score test for the
#' proportion." (Alan Agresti, who wrote the original R code)
#'
#' Inputs for \code{success}, \code{total}, and \code{level}
#' are vectorized.
#'
#' @param success Success count.
#' @param total Total count.
#' @param level Optional. Confidence level. Defaults to 0.95.
#' @param return_midpoint Optional. Return midpoint of confidence
#'   interval? Defaults to \code{FALSE}.
#'
#' @return Data frame:
#'   * \code{success} Success count
#'   * \code{total} Total count
#'   * \code{estimate} Proportion
#'   * \code{conf.low} Lower bound of the confidence interval.
#'   * \code{conf.high} Upper bound of the confidence interval.
#'   * \code{midpoint} Mid-point of the confidence interval
#'     (for \code{return_midpoint = TRUE}).
#'   * \code{level} Confidence level.
#' @export
#'
#' @seealso
#' \url{https://users.stat.ufl.edu/~aa/cda/R/one-sample/R1/index.html}
#'
#' Agresti A, Coull BA. Approximate is better than "exact" for
#' interval estimation of binomial proportions. Am Stat 1998;52:119-126.
#' \doi{10.2307/2685469}
#'
#' Brown LD, Cai TT, DasGupta A. Interval estimation for a
#' binomial proportion (with discussion). Stat Sci 2001;16:101-133.
#' \doi{10.1214/ss/1009213286}
#'
#' @examples
#' scoreci(success = 5, total = 10)
#' scoreci(success = c(5:10), total = 10, level = 0.9)
scoreci <- function(success, total, level = 0.95, return_midpoint = FALSE) {
  zalpha <- abs(stats::qnorm((1 - level) / 2))
  estimate <- success / total
  bound <- (zalpha * ((estimate * (1 - estimate) + (zalpha**2) / (4 * total)) / total)**(1 / 2)) /
    (1 + (zalpha**2) / total)
  midpoint <- (estimate + (zalpha**2) / (2 * total)) / (1 + (zalpha**2) / total)

  conf.low <- midpoint - bound
  conf.high <- midpoint + bound

  if (return_midpoint) {
    data.frame(success, total, estimate, conf.low, conf.high, midpoint, level)
  } else {
    data.frame(success, total, estimate, conf.low, conf.high, level)
  }
}
