#' Design A Descriptive Table
#'
#' @description This function generates a \code{design} table from which
#'   \code{\link[rifttable]{rifttable}} can generate a descriptive table.
#'
#' @param data Data set
#' @param ... Optional: Variables to include or exclude (using \code{-variable})
#' @param by Optional: Stratification variable. Typically the exposure.
#' @param total Optional: Whether to add the total count at the beginning.
#'   Defaults to \code{TRUE}.
#'
#' @return \code{design} tibble that can be passed on to
#'   \code{\link[rifttable]{rifttable}}.
#' @export
#'
#' @examples
#' # Data preparation
#' cars <- tibble::as_tibble(mtcars) |>
#' dplyr::mutate(
#'   gear = factor(
#'     gear,
#'     levels = 3:5,
#'     labels = c("Three", "Four", "Five")),
#'   hp_categorical = dplyr::if_else(
#'     hp > 200,
#'     true = ">200 hp",
#'     false = "<=200 hp")) |>
#'   labelled::set_variable_labels(
#'     mpg = "Miles per gallon",
#'     hp = "Horsepower",
#'     hp_categorical = "Horsepower",
#'     am = "Automatic transmission",
#'     gear = "Gears")
#'
#' # Generate table "design"
#' design <- cars |>
#'   table1_design(mpg, hp, hp_categorical, am, by = gear)
#'
#' # Use "design" to create a descriptive table
#' design |>
#'   rifttable(data = cars, diff_digits = 0)
table1_design <- function(
    data,
    ...,
    by = NULL,
    total = TRUE) {
  olddata <- data
  data <- data |>
    dplyr::select(!!!rlang::enquos(...))
  if(ncol(data) == 0)
    data <- olddata
  variables <- colnames(data)
  if(!missing(by))
    variables <- variables[variables != deparse(substitute(by))]

  design <- tibble::tibble(variable = variables) |>
    dplyr::mutate(
      type = purrr::map_chr(
        .x = .data$variable,
        .f = ~data |>
          dplyr::select(var = dplyr::one_of(.x)) |>
          dplyr::pull(var) |>
          class()),
      levels = purrr::map(
        .x = .data$variable,
        .f = ~data |>
          dplyr::select(var = dplyr::one_of(.x)) |>
          dplyr::pull(var) |>
          na.exclude() |>
          unique())) |>
    dplyr::left_join(
      data |>
        labelled::var_label(unlist = TRUE) |>
        tibble::as_tibble(rownames = "variable") |>
        dplyr::rename(var_label = .data$value),
      by = "variable") |>
    dplyr::mutate(
      nlevels = purrr::map_int(.x = levels, .f = length),
      variable_type = dplyr::case_when(
        .data$type %in% c(
          "character", "factor", "ordered") ~
          "categorical",
        purrr::map_lgl(
          .x = .data$levels,  # this is TRUE also for "logical" variable
          .f = ~all(c(0, 1) %in% .x)) ~
          "binary",
        .data$type == "numeric" ~
          "numeric",
        TRUE ~
          "ERROR-undefined"),
      has_na = purrr::map_lgl(
        .x = .data$variable,
        .f = ~data |>
          dplyr::select(var = dplyr::one_of(.x)) |>
          dplyr::pull(var) |>
          is.na() |>
          any()),
      nlevels = dplyr::if_else(
        .data$variable_type == "categorical",
        true = .data$nlevels + 1 + .data$has_na,
        false = 1 + .data$has_na),
      levels = purrr::pmap(
        .l = list(.data$variable_type, .data$variable, .data$levels),
        .f = ~{
          if(..1 == "categorical")
            c(..2, as.character(..3))
          else
            as.character(..3)
          })) |>
    tidyr::uncount(
      weights = nlevels,
      .remove = FALSE) |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(
      var_level = dplyr::case_when(
        .data$variable_type == "categorical" &
          .data$has_na == TRUE &
          dplyr::row_number() == .data$nlevels ~
          "_NA_",
        .data$variable_type != "categorical" &
          .data$has_na == TRUE &
          dplyr::row_number() == 2 ~
          "_NA_",
        .data$variable_type == "categorical" ~
          purrr::map2_chr(
            .x = .data$levels,
            .y = dplyr::row_number(),
            .f = ~.x[.y]),
        TRUE ~
          ""),
      outcome = dplyr::case_when(
        .data$var_level == "_NA_" ~
          paste0(.data$variable, "@_NA_"),
        .data$variable_type == "categorical" &
          dplyr::row_number() == 1 ~
          "",
        .data$variable_type == "categorical" &
          dplyr::row_number() > 1 ~
          paste0(.data$variable, "@", .data$var_level),
        TRUE ~
          .data$variable),
      type = dplyr::case_when(
        .data$var_level == "_NA_" ~
          "outcomes",
        .data$variable_type == "categorical" &
          dplyr::row_number() == 1 ~
          "",
        .data$variable_type == "categorical" &
          dplyr::row_number() > 1 ~
          "outcomes (risk)",
        .data$variable_type == "binary" ~
          "outcomes (risk)",
        .data$variable_type == "numeric" ~
          "median (iqr)",
        TRUE ~
          "ERROR"),
      label = dplyr::case_when(
        .data$var_level == "_NA_" ~
          "  Unknown",
        .data$variable_type == "categorical" &
          dplyr::row_number() == 1 &
          .data$var_label != "" ~
          .data$var_label,
        .data$variable_type == "categorical" &
          dplyr::row_number() == 1 &
          .data$var_label == "" ~
          .data$variable,
        .data$variable_type == "categorical" &
          dplyr::row_number() > 1 ~
          paste0("  ", .data$var_level),
        .data$var_label != "" ~
          .data$var_label,
        .data$var_label == "" ~
          .data$variable),
      na_rm = .data$has_na &
        .data$var_level != "_NA_" &
        !(.data$variable_type == "categorical" &
            dplyr::row_number() == 1)) |>
    dplyr::ungroup() |>
    dplyr::select("label", "outcome", "type", "na_rm")
  # Have "na_rm" column only in "design" if >= 1 variable has missing data
  if(!any(design$na_rm)) {
    design <- design |> dplyr::select(-"na_rm")
  }
  if(total == TRUE) {
    design <- dplyr::bind_rows(
      tibble::tibble(label = "N", outcome = "any", type = "total"),
      design)
  }
  if(!missing(by)) {
    design$exposure <- deparse(substitute(by))
  }
  return(design)
}

