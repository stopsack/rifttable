#' Format Results of Regression Models
#'
#' @param data Data set
#' @param digits Number of digits to round estimates to
#' @param ratio_digits_decrease Ratios to round less
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param nmin Suppress counts below
#' @param pattern Regex pattern for removing regression terms
#' @param xlevels Strata of exposure variable
#' @param reference Label for reference category
#' @param fit Fitted, tidied model
#' @param suppress How to supress low counts
#' @param multiply Factor
#' @param reference_label Label for reference group
#' @param percent Whether to add percentage sign
#' @param conflimit_check Additional nonconvergence check based on confidence
#'   limits
#'
#' @return Tibble
#' @noRd
format_regression_results <- function(
    fit,
    data,
    suppress,
    is_trend,
    multiply,
    digits,
    ratio_digits_decrease,
    pattern,
    xlevels,
    reference,
    nmin,
    to,
    reference_label,
    percent = FALSE,
    conflimit_check = FALSE) {  # only needed for RR/RD models
  fit <- fit %>%
    dplyr::select(
      .data$term,
      .data$estimate,
      .data$conf.low,
      .data$conf.high) %>%
    dplyr::mutate(
      nonconverg = (.data$conf.low == 0 &
                      .data$conf.high == Inf)) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(
        .data$estimate,
        .data$conf.low,
        .data$conf.high),
      .funs = ~format_round(
        . * multiply,
        digits = digits,
        ratio_digits_decrease = ratio_digits_decrease)) %>%
    dplyr::full_join(
      counts_per_stratum(
        data = data,
        suppress = suppress,
        is_trend) %>%
        dplyr::mutate(
          .exposure = paste0(
            ".exposure",
            .data$.exposure)),
      by = c(term = ".exposure")) %>%
    dplyr::filter(stringr::str_detect(
      string = .data$term,
      pattern = pattern)) %>%
    dplyr::mutate(
      .exposure = stringr::str_remove(
        string = .data$term,
        pattern = pattern))

  if(is_trend == TRUE) {
    fit <- fit %>%
      dplyr::slice(1) %>%
      dplyr::mutate(.exposure = "Trend")
  } else {
    fit <- fit %>%
      dplyr::left_join(
        x = tibble::tibble(.exposure = xlevels),
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
          paste0(
            .data$estimate,
            dplyr::if_else(
              percent,
              true = "%",
              false = ""),
            " (",
            .data$conf.low,
            to,
            .data$conf.high,
            ")")),
      res = dplyr::if_else(
        (is.na(.data$estimate) |
           (.data$conf.low == .data$conf.high &
              .data$conf.low == .data$estimate &
              conflimit_check == TRUE)) &
          dplyr::row_number() == 1,
        true = paste(reference, reference_label),
        false = .data$res),
      res = dplyr::if_else(
        .data$.per_stratum < nmin,
        true = paste0("-- (<", nmin, ")"),
        false = .data$res)) %>%
    dplyr::select(.data$.exposure, .data$res)
}
