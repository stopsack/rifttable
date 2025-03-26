#' Check if package is installed
#'
#' This function tests if a package is installed. Its main purpose is for use
#' together with mocking in testthat.
#'
#' @param package Package name.
#'
#' @return Logical.
#' @noRd
#'
#' @examples
#' is_package_installed("dplyr")
is_package_installed <- function(package) {
  requireNamespace(package, quietly = TRUE)
}
