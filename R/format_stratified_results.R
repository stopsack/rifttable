#' Format Results of Stratified Tables
#'
#' @param results Stratified results output
#' @param data Data set
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#' @param nmin Suppress counts below
#' @param suppress How to supress low counts
#'
#' @return Tibble
#' @noRd
format_stratified_results <- function(
    results,
    data,
    to,
    suppress,
    nmin,
    is_trend) {
  results %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      counts_per_stratum(
        data = data,
        suppress = suppress,
        is_trend),
      by = ".exposure") %>%
    dplyr::mutate(
      res = dplyr::if_else(
        stringr::str_remove(
          string = .data$res,
          pattern = "%") %in%
          c("NaN", "NA", "NaN (NA)",
            paste0("NA (NA", to, "NA)"),
            paste0("NaN (NaN", to, "NaN)")),
        true = "--",
        false = .data$res),
      res = dplyr::case_when(
        stringr::str_detect(
          string = .data$res,
          pattern = stringr::fixed("(NaN)")) ~
          paste0(
            stringr::str_remove(
              string = .data$res,
              pattern = stringr::fixed("(NaN)")),
            "(--)"),
        stringr::str_detect(
          string = .data$res,
          pattern = stringr::fixed("(NaN%)")) ~
          paste0(
            stringr::str_remove(
              string = .data$res,
              pattern = stringr::fixed("(NaN%)")),
            "(--)"),
        TRUE ~ .data$res),
      res = dplyr::case_when(
        .data$.per_stratum < nmin ~
          paste0("-- (<", nmin, ")"),
        TRUE ~ .data$res)) %>%
    dplyr::select(-.data$.per_stratum)
}
