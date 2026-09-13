test_that("layout as columns works", {
  data(breastcancer, package = "risks")

  design <- tibble::tibble(
    type = c("outcomes/total", "risk"),
  ) |>
    dplyr::mutate(
      exposure = "stage",
      outcome = "death"
    )

  object <- rifttable(
    design = design,
    data = breastcancer,
    layout = "cols",
    overall = TRUE
  )
  expected <- tibble::tribble(
    ~Stage,      ~`outcomes/total`, ~risk,
    "Overall",   "54/192",          "0.28",
    "Stage I",   "7/67",            "0.10",
    "Stage II",  "26/96",           "0.27",
    "Stage III", "21/29",           "0.72"
  )
  expect_equal(
    object = object,
    expected = expected
  )

  object <- design |>
    dplyr::mutate(label = "") |>
    rifttable(
      data = breastcancer,
      layout = "cols",
      overall = TRUE
    )
  expected <- tibble::tribble(
    ~.exposure,  ~`1_`,    ~`2_`,
    "Overall",   "54/192", "0.28",
    "Stage I",   "7/67",   "0.10",
    "Stage II",  "26/96",  "0.27",
    "Stage III", "21/29",  "0.72"
  )
  expect_equal(
    object = object,
    expected = expected
  )
})


test_that("variables labels appear in column names", {
  data(breastcancer, package = "risks")

  breastcancer <- breastcancer |>
    dplyr::mutate(Low = receptor)

  expect_equal(
    object = tibble::tibble(
      type = "total",
      exposure = "receptor",
      outcome = "death"
    ) |>
      rifttable(data = breastcancer),
    expected = tibble::tribble(
      ~`Hormone receptor`, ~High, ~Low,
      "total",             "144", "48"
    )
  )
})

test_that("variables or labels that are values get caught", {
  data(breastcancer, package = "risks")

  breastcancer <- breastcancer |>
    dplyr::mutate(Low = receptor)

  attr(breastcancer$Low, which = "label") <- NULL
  expect_equal(
    object = tibble::tibble(
        type = "total",
        exposure = "Low",
        outcome = "death"
      ) |>
      rifttable(data = breastcancer) |>
      colnames() |>
      dplyr::first(),
    expected = "By Low"
  )

  attr(breastcancer$Low, which = "label") <- ""
  expect_equal(
    object = tibble::tibble(
      type = "total",
      exposure = "Low",
      outcome = "death"
    ) |>
      rifttable(data = breastcancer) |>
      colnames() |>
      dplyr::first(),
    expected = "By Low"
  )

  attr(breastcancer$Low, which = "label") <- NA
  expect_equal(
    object = tibble::tibble(
      type = "total",
      exposure = "Low",
      outcome = "death"
    ) |>
      rifttable(data = breastcancer) |>
      colnames() |>
      dplyr::first(),
    expected = "By Low"
  )

  attr(breastcancer$Low, which = "label") <- "Low"
  expect_equal(
    object = tibble::tibble(
      type = "total",
      exposure = "Low",
      outcome = "death"
    ) |>
      rifttable(data = breastcancer) |>
      colnames() |>
      dplyr::first(),
    expected = "By Low"
  )
})


test_that("labels are added if attributes are empty", {
  data(breastcancer, package = "risks")

  attr(breastcancer$receptor, which = "label") <- ""
  expect_equal(
    object = tibble::tibble(
      type = "total",
      exposure = "receptor",
      outcome = "death"
    ) |>
      rifttable(data = breastcancer) |>
      colnames() |>
      dplyr::first(),
    expected = "receptor"
  )

  attr(breastcancer$receptor, which = "label") <- NA
  expect_equal(
    object = tibble::tibble(
      type = "total",
      exposure = "receptor",
      outcome = "death"
    ) |>
      rifttable(data = breastcancer) |>
      colnames() |>
      dplyr::first(),
    expected = "receptor"
  )
})
