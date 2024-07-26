test_that("layout as columns works", {
  data(breastcancer, package = "risks")

  design <- tibble::tibble(
    type = c("outcomes/total", "risk"),
  ) %>%
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

  object <- design %>%
    dplyr::mutate(label = "") %>%
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
