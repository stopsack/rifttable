data("breastcancer", package = "risks")
df <- breastcancer |>
  dplyr::mutate(
    continuous = 1:dplyr::n(),
    receptor = dplyr::if_else(dplyr::row_number() %in% 9:11, NA, receptor),
    stage_num_orig = as.numeric(stage),
    stage = dplyr::if_else(dplyr::row_number() %in% 29:31, NA, stage),
    death = dplyr::if_else(dplyr::row_number() %in% 99:101, NA, death),
    stage_num = as.numeric(stage)
  )


test_that("Trend works anywhere", {
  result <- tibble::tribble(
    ~label,   ~exposure, ~trend,      ~outcome,     ~type,
    "N",      "stage",   "stage_num", NA,           "total",
    "deaths", "stage",   "stage_num", "death",      "outcomes",
    "RD",     "stage",   "stage_num", "death",      "rd",
    "diff",   "stage",   "stage_num", "continuous", "diff"
  ) |>
    rifttable(data = df)
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 6)
  expect_equal(
    names(result),
    c("Stage", "Stage I", "Stage II", "Stage III", "NA", "Trend")
  )

  result <- tibble::tribble(
    ~label,   ~exposure, ~trend,      ~outcome,     ~type,
    "N",      NA,        NA,          NA,           "total",
    "deaths", "stage",   "stage_num", "death",      "outcomes",
    "RD",     "stage",   "stage_num", "death",      "rd",
    "diff",   "stage",   "stage_num", "continuous", "diff"
  ) |>
    rifttable(data = df)
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 7)
  expect_equal(
    names(result),
    c("Stage", "Overall", "Stage I", "Stage II", "Stage III", "NA", "Trend")
  )

  result <- tibble::tribble(
    ~label,   ~exposure, ~trend,      ~outcome,     ~type,
    "N",      NA,        NA,          NA,           "total",
    "deaths", "stage",   "stage_num", "death",      "outcomes",
    "RD",     "stage",   "stage_num", "death",      "rd",
    "diff",   "stage",   "stage_num", "continuous", "diff"
  ) |>
    rifttable(
      data = df,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 4)
  expect_equal(ncol(result), expected = 6)
  expect_equal(
    names(result),
    c("Stage", "Overall", "Stage I", "Stage II", "Stage III", "Trend")
  )

  result <- tibble::tribble(
    ~label,   ~exposure, ~trend,      ~outcome,     ~type,
    "deaths", "",        "stage_num", "death",      "outcomes",
    "RD",     "stage",   "stage_num", "death",      "rd",
    "OR",     "",        "stage_num", "death",      "or"
  ) |>
    rifttable(data = df)
  expect_equal(nrow(result), expected = 3)
  expect_equal(ncol(result), expected = 6)
  expect_equal(
    names(result),
    c("Stage", "Overall", "Stage I", "Stage II", "Stage III", "Trend")
  )

  result <- tibble::tribble(
    ~label,   ~exposure, ~trend,      ~outcome,     ~type,
    "OR",     "",        "stage_num", "death",      "or",
    "deaths", "stage",   "stage_num", "death",      "outcomes",
    "RD",     "stage",   "stage_num", "death",      "rd"
  ) |>
    rifttable(data = df)
  expect_equal(nrow(result), expected = 3)
  expect_equal(ncol(result), expected = 6)
  expect_equal(
    names(result),
    c("Stage", "Stage I", "Stage II", "Stage III", "NA", "Trend")
  )

  result <- tibble::tribble(
    ~label,   ~exposure, ~trend,      ~outcome,     ~type,
    "OR",     "",        "stage_num", "death",      "or",
    "deaths", "stage",   "stage_num", "death",      "outcomes",
    "RD",     "stage",   "stage_num", "death",      "rd"
  ) |>
    rifttable(
      data = df,
      overall = TRUE,
      exposure_levels = "nona"
    )
  expect_equal(nrow(result), expected = 3)
  expect_equal(ncol(result), expected = 6)
  expect_equal(
    names(result),
    c("Summary", "Overall", "Stage I", "Stage II", "Stage III", "Trend")
  )
})
