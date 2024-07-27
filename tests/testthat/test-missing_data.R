data("breastcancer", package = "risks")
df <- breastcancer |>
  dplyr::mutate(
    continuous = 1:dplyr::n(),
    receptor = dplyr::if_else(dplyr::row_number() %in% 9:11, NA, receptor),
    stage = dplyr::if_else(dplyr::row_number() %in% 29:31, NA, stage),
    death = dplyr::if_else(dplyr::row_number() %in% 99:101, NA, death),
    allempty = NA_real_,
    allempty_lgl = allempty == 1
  )


test_that("Missing exposure level is handled correctly by 'total'", {
  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,     ~type,
    "N",      NA,         NA,           "total",
    "deaths", "receptor", "death",      "outcomes",
    "mean",   "receptor", "continuous", "mean",
    "diff",   "receptor", "continuous", "diff"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "noempty"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 5)
  expect_equal(
    names(result),
    c("Hormone receptor", "Overall", "High", "Low", "NA")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,     ~type,
    "N",      NA,         NA,           "total",
    "deaths", "receptor", "death",      "outcomes",
    "mean",   "receptor", "continuous", "mean",
    "diff",   "receptor", "continuous", "diff"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Hormone receptor", "Overall", "High", "Low")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,     ~type,
    "N",      NA,         NA,           "total",
    "deaths", "receptor", "death",      "outcomes",
    "mean",   "receptor", "continuous", "mean",
    "diff",   "receptor", "continuous", "diff"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "all"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 5)
  expect_equal(
    names(result),
    c("Hormone receptor", "Overall", "High", "Low", "NA")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,     ~type,
    "N",      NA,         NA,          "total",
    "deaths", "receptor", "death",      "outcomes",
    "mean",   "receptor", "continuous", "mean",
    "diff",   "receptor", "continuous", "diff") |>
    rifttable(
      data = df,
      overall = TRUE,
      exposure_levels = "all"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 5)
  expect_equal(
    names(result),
    c("Summary", "Overall", "High", "Low", "NA")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,     ~type,
    "N",      "receptor", "death",      "total",
    "deaths", "receptor", "death",      "outcomes",
    "mean",   "receptor", "continuous", "mean",
    "diff",   "receptor", "continuous", "diff"
  ) |>
    rifttable(
      data = df,
      overall = TRUE,
      exposure_levels = "all"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 5)
  expect_equal(
    names(result),
    c("Summary", "Overall", "High", "Low", "NA")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,     ~type,
    "N",      "receptor", "death",      "total",
    "deaths", "receptor", "death",      "outcomes",
    "mean",   "receptor", "continuous", "mean",
    "diff",   "receptor", "continuous", "diff"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "all"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Hormone receptor", "High", "Low", "NA")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,     ~type,
    "N",      "receptor", "death",      "total",
    "deaths", "receptor", "death",      "outcomes",
    "mean",   "receptor", "continuous", "mean",
    "diff",   "receptor", "continuous", "diff"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 3)
  expect_equal(
    names(result),
    c("Hormone receptor", "High", "Low")
  )
})

test_that("Missing exposure level is handled correctly by 'blank'", {
  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome, ~type,
    "Blank",  "receptor", "death",  ""
  ) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 1)
  expect_equal(ncol(result), expected = 3)
  expect_equal(
    names(result),
    c("Hormone receptor", "High", "Low")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome, ~type,
    "Blank",  NA,         NA,       ""
  ) |>
    rifttable(data = df)
  expect_equal(nrow(result), expected = 1)
  expect_equal(ncol(result), expected = 2)
  expect_equal(
    names(result),
    c("Summary", "Overall")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome, ~type,
    "Blank",  NA,         NA,       "",
    "N",      "receptor", NA,       "total"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 2)
  expect_equal(ncol(result), expected = 3)
  expect_equal(
    names(result),
    c("Hormone receptor", "High", "Low")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome, ~type,
    "Blank",  NA,         NA,       "",
    "N",      "receptor", NA,       "total"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "all"
    )
  expect_equal(nrow(result), expected = 2)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Hormone receptor", "High", "Low", "NA")
  )
})

test_that("All-NA logical variable variable is handled correctly", {
  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,            ~type,
    "N",      NA,         NA,                  "total",
    "deaths", "receptor", "death",             "outcomes",
    "mean",   "receptor", "continuous",        "mean",
    "empty",  "receptor", "allempty_lgl",      "outcomes",
    "  NAs",  "receptor", "allempty_lgl@_NA_", "outcomes"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 5)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Hormone receptor", "Overall", "High", "Low")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,       ~type,
    "N",      NA,         NA,             "total",
    "deaths", "receptor", "death",        "outcomes",
    "mean",   "receptor", "continuous",   "mean",
    "empty",  "receptor", "allempty_lgl", "outcomes"
  ) |>
    dplyr::mutate(na_rm = TRUE) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Hormone receptor", "Overall", "High", "Low")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,       ~type,
    "N",      NA,         NA,             "total",
    "deaths", "receptor", "death",        "outcomes",
    "mean",   "receptor", "continuous",   "mean",
    "empty",  "receptor", "allempty_lgl", "outcomes"
  ) |>
    dplyr::mutate(na_rm = TRUE) |>
    rifttable(
      data = df,
      overall = TRUE,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Summary", "Overall", "High", "Low")
  )
})

test_that("All-NA real variable variable is handled correctly", {
  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,        ~type,
    "N",      NA,         NA,              "total",
    "deaths", "receptor", "death",         "outcomes",
    "mean",   "receptor", "continuous",    "mean",
    "empty",  "receptor", "allempty",      "outcomes",
    "  NAs",  "receptor", "allempty@_NA_", "outcomes"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 5)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Hormone receptor", "Overall", "High", "Low")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,     ~type,
    "N",      NA,         NA,           "total",
    "deaths", "receptor", "death",      "outcomes",
    "mean",   "receptor", "continuous", "mean",
    "empty",  "receptor", "allempty",   "outcomes"
  ) |>
    dplyr::mutate(na_rm = TRUE) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Hormone receptor", "Overall", "High", "Low")
  )

  result <- tibble::tribble(
    ~label,   ~exposure,  ~outcome,       ~type,
    "N",      NA,         NA,             "total",
    "deaths", "receptor", "death",        "outcomes",
    "mean",   "receptor", "continuous",   "mean",
    "empty",  "receptor", "allempty",     "outcomes"
  ) |>
    dplyr::mutate(na_rm = TRUE) |>
    rifttable(
      data = df,
      overall = TRUE,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 4)
  expect_equal(
    names(result),
    c("Summary", "Overall", "High", "Low")
  )
})

test_that("Missing outcome or outcome of wrong type gets detected", {
  expect_no_message(
    tibble::tribble(
      ~outcome,     ~type,
      NA,           "total",
      "continuous", "median"
    ) |>
      rifttable(data = df)
  )

  expect_error(
    tibble::tribble(
      ~outcome, ~type,
      NA,       "cases/controls"
    ) |>
      rifttable(data = df),
    "The 'design' must contain an 'outcome' variable that exists in the 'data'"
  )

  expect_error(
    tibble::tribble(
      ~outcome, ~type,
      "stage",  "cases/controls"
    ) |>
      rifttable(data = df),
    "Outcome variable 'stage' must be binary with levels"
  )

  expect_error(
    tibble::tribble(
      ~outcome,   ~type,
      "receptor", "median"
    ) |>
      rifttable(data = df),
    "Outcome variable 'receptor' must be continuous"
  )
})

test_that(
  desc = "Logical exposure has 'all' levels shown",
  code = {
    expect_equal(
      object = tibble::tibble(
        exposure = "allempty_lgl",
        outcome = "death",
        type = "outcomes"
      ) |>
        rifttable(
          data = df,
          exposure_levels = "all"
        ),
      expected = tibble::tibble(
        allempty_lgl = "outcomes",
        `FALSE` = "0",
        `TRUE` = "0",
        `NA` = "--"
      )
    )
  }
)
