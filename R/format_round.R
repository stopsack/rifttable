#' Trim, format, and round a number
#'
#' @param x Number
#' @param digits Digits to round to
#' @param ratio_digits_decrease Named vector of the format, \code{c(`10` = -2)}
#'   to reduce the number of rounding digits by 2 for ratios greater than 10,
#'   for example.
#'
#' @return Character
#'
#' @noRd
format_round <- function(
    x,
    digits,
    ratio_digits_decrease = NULL
) {
  if (!is.null(ratio_digits_decrease)) {
    if (!is.numeric(ratio_digits_decrease)) {
      stop(
        paste0(
          "Values of 'ratio_digits_decrease' for rounding, if provided, must ",
          "be numeric. '",
          paste(ratio_digits_decrease, collapse = ", "),
          "' is not (all) numeric."
        )
      )
    }
    if (any(is.na(suppressWarnings(as.numeric(names(ratio_digits_decrease)))))) {
      stop(
        paste0(
          "Names of 'ratio_digits_decrease' for rounding, if provided, must ",
          "be convertible into numbers. '",
          paste(names(ratio_digits_decrease), collapse = ", "),
          "' are not (all) numbers."
        )
      )
    }
    add_zero <- 0
    names(add_zero) <- "0"
    ratio_digits_decrease <- c(ratio_digits_decrease, add_zero)
    ratio_digits_decrease <- ratio_digits_decrease[
      as.character(
        sort(
          as.numeric(
            names(ratio_digits_decrease)
          )
        )
      )
    ]
    digits_decrease <- sapply(X = x, function(x) {
      if (!is.na(suppressWarnings(as.numeric(x)))) {
        selected_digit <- ratio_digits_decrease[
          which(as.numeric(names(ratio_digits_decrease)) <= x)
        ]
        selected_digit[length(selected_digit)]
      } else {
        0
      }
    })
    if (!is.numeric(digits_decrease)) {
      digits_decrease <- 0
    }
    digits <- pmax(digits + digits_decrease, 0)
  }
  mapply(
    FUN = function(x, digits) {
      x_numeric <- suppressWarnings(as.numeric(x))
      if (!is.na(x_numeric)) {
        format(
          round(
            x_numeric,
            digits = digits
          ),
          nsmall = digits,
          trim = TRUE,
          scientific = FALSE
        )
      } else {
        as.character(x)
      }
    },
    x,
    digits
  )
}
