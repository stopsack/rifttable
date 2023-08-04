data("breastcancer", package = "risks")
df <- breastcancer |>
  dplyr::mutate(
    continuous = 1:dplyr::n() / 100,
    receptor = dplyr::if_else(dplyr::row_number() %in% 9:11, NA, receptor),
    stage = dplyr::if_else(dplyr::row_number() %in% 29:31, NA, stage),
    death = dplyr::if_else(dplyr::row_number() %in% 99:101, NA, death))

test_that("Ratio models for continuous outcomes work", {
  results <- tibble::tibble(
    exposure = "receptor",
    outcome = "continuous",
    type = c("irrrob", "fold", "foldlog")) %>%
    rifttable(data = df)
  expect_true(all(results$High == "1 (reference)"))
  expect_equal(results$Low[[1]], expected = "0.64 (0.52, 0.80)")
  expect_equal(results$Low[[2]], expected = "0.64 (0.49, 0.80)")
  expect_equal(results$Low[[3]], expected = "0.56 (0.41, 0.75)")
})
