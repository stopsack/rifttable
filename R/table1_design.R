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
#' @param empty_levels Optional: Whether to include empty levels of factor
#'   variables. Defaults to \code{FALSE}.
#' @param na_always Optional: Whether to add the count of missing values for
#'   each variable, even if there are none. Defaults to \code{FALSE}, i.e.,
#'   the count of missing values will only be shown if there are any.
#' @param na_label Label for count of missing values. Defaults to
#'   \code{"Unknown"}.
#' @param continuous_type Estimator (\code{type} in
#'   \code{\link[rifttable]{rifttable}} \code{design}) for continuous variables.
#'   Defaults to \code{"median (iqr)"}.
#' @param binary_type Estimator (\code{type} in
#'   \code{\link[rifttable]{rifttable}} \code{design}) for binary variables and
#'   strata of categorical variables. Defaults to \code{"outcomes (risk)"}
#'   (count and column proportion).
#'
#' @return \code{design} tibble that can be passed on to
#'   \code{\link[rifttable]{rifttable}}. Contains an attribute \code{rt_data}
#'   so that the dataset does not have to be provided to
#'   \code{\link[rifttable]{rifttable}} another time.
#' @export
#'
#' @examples
#' # Data preparation
#' cars <- tibble::as_tibble(mtcars) |>
#'   dplyr::mutate(
#'     gear = factor(
#'       gear,
#'       levels = 3:5,
#'       labels = c("Three", "Four", "Five")
#'     ),
#'     # Categorical version of "hp", shows each category
#'     hp_categorical = dplyr::if_else(
#'       hp >= 200,
#'       true = "200+ hp",
#'       false = "<200 hp"
#'     ),
#'     # Binary version of "hp", shows the TRUEs
#'     hp_binary = hp >= 200
#'   )
#' # Label some variables. Better alternative: labelled::set_variable_labels()
#' attr(cars$hp, "label") <- "Horsepower"
#' attr(cars$hp_categorical, "label") <- "Horsepower"
#' attr(cars$hp_binary, "label") <- "200+ hp"
#' attr(cars$am, "label") <- "Automatic transmission"
#' attr(cars$gear, "label") <- "Gears"
#'
#' # Generate table "design"
#' design <- cars |>
#'   table1_design(
#'     hp, hp_categorical, hp_binary, mpg, am,
#'     by = gear
#'   )
#'
#' # Use "design" to create a descriptive table.
#' design |>
#'   rifttable(diff_digits = 0)
#'
#' # Obtain a formatted table
#' design |>
#'   rifttable(diff_digits = 0) |>
#'   rt_gt()
table1_design <- function(
    data,
    ...,
    by = NULL,
    total = TRUE,
    empty_levels = FALSE,
    na_always = FALSE,
    na_label = "Unknown",
    continuous_type = "median (iqr)",
    binary_type = "outcomes (risk)"
) {
  olddata <- data
  data <- data |>
    dplyr::select(!!!rlang::enquos(...))
  if (ncol(data) == 0) {
    data <- olddata
  }
  if (!missing(by)) {
    if (deparse(substitute(by)) %in% names(data)) {
      data <- data |>
        dplyr::select(!{{ by }})
    }
  }

  label_list <- purrr::map(
    .x = data,
    .f = attr,
    which = "label"
  )
  design <- tibble::tibble(
    variable = names(label_list),
    var_label = as.character(label_list),
    type = purrr::map_chr(
      .x = data,
      .f = class
    )
  ) |>
    dplyr::mutate(
      levels = purrr::map(
        .x = .data$variable,
        .f = \(x) sort(unique(stats::na.exclude(data[[x]])))
      )
    )
  if (empty_levels == TRUE) {
    design <- design |>
      dplyr::mutate(
        levels_f = purrr::map(
          .x = .data$variable,
          .f = \(x) levels(data[[x]])
        ),
        levels = dplyr::if_else(
          .data$type == "factor",
          true = .data$levels_f,
          false = .data$levels
        )
      )
  }
  design <- design |>
    dplyr::mutate(
      has_na = purrr::map_lgl(
        .x = .data$variable,
        .f = \(x) anyNA(data[, x])
      ),
      variable_type = dplyr::case_when(
        .data$type %in% c(
          "character", "factor", "ordered"
        ) ~
          "categorical",
        purrr::map_lgl(
          .x = .data$levels, # this is TRUE also for "logical" variable
          .f = \(x) all(stats::na.omit(x) %in% c(0, 1))
        ) ~
          "binary",
        .data$type %in% c("numeric", "integer") ~
          "numeric",
        TRUE ~
          "ERROR-undefined"
      ),
      outcome = purrr::pmap(
        .l = list(
          .data$variable_type,
          .data$variable,
          .data$levels
        ),
        .f = \(variable_type, variable, levels) {
          if (variable_type == "categorical") {
            c(
              "",
              paste(variable, levels, sep = "@"),
              paste0(variable, "@_NA_")
            )
          } else {
            c(variable, paste0(variable, "@_NA_"))
          }
        }
      )
    ) |>
    tidyr::unnest_longer(col = "outcome") |>
    dplyr::filter(
      !(.data$has_na == FALSE &
          na_always == FALSE &
          stringr::str_detect(
            string = .data$outcome,
            pattern = "@_NA_$"
          )
      )
    ) |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_detect(
          string = .data$outcome,
          pattern = "@_NA_$"
        ) ~
          "outcomes",
        .data$variable_type == "categorical" &
          dplyr::row_number() == 1 ~
          "",
        .data$variable_type == "categorical" &
          dplyr::row_number() > 1 ~
          binary_type,
        .data$variable_type == "binary" ~
          binary_type,
        .data$variable_type == "numeric" ~
          continuous_type,
        TRUE ~
          "ERROR"
      ),
      label = dplyr::case_when(
        stringr::str_detect(
          string = .data$outcome,
          pattern = "@_NA_$"
        ) ~
          paste0("  ", na_label),
        .data$variable_type == "categorical" &
          dplyr::row_number() == 1 &
          !(.data$var_label %in% c("", "NULL")) ~
          .data$var_label,
        .data$variable_type == "categorical" &
          dplyr::row_number() == 1 &
          !(.data$var_label %in% c("", "NULL")) ~
          .data$variable,
        .data$variable_type == "categorical" &
          dplyr::row_number() > 1 ~
          paste0(
            "  ",
            stringr::str_remove(
              string = .data$outcome,
              pattern = paste0("^", .data$variable, "@")
            )
          ),
        !(.data$var_label %in% c("", "NULL")) ~
          .data$var_label,
        .data$var_label %in% c("", "NULL") ~
          .data$variable
      ),
      na_rm = .data$has_na &
        !stringr::str_detect(
          string = .data$outcome,
          pattern = "@_NA_$"
        ) &
        !(.data$variable_type == "categorical" &
          dplyr::row_number() == 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::select("label", "outcome", "type", "na_rm")
  # Have "na_rm" column only in "design" if >= 1 variable has missing data
  if (!any(design$na_rm)) {
    design <- design |>
      dplyr::select(-"na_rm")
  }
  if (total == TRUE) {
    design <- dplyr::bind_rows(
      tibble::tibble(
        label = "N",
        outcome = "",
        type = "total"
      ),
      design
    )
  }
  if (!missing(by)) {
    design$exposure <- stringr::str_remove_all(
      string = deparse(substitute(by)),
      pattern = "[\"\']"
    )
  }
  if (length(rlang::enquos(...)) > 0) {
    data_for_attr <- olddata |>
      dplyr::select(!!!rlang::enquos(...), {{ by }})
  } else {
    data_for_attr <- olddata
  }
  attr(x = design, which = "rt_data") <- data_for_attr
  return(design)
}
