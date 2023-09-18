data("breastcancer", package = "risks")
df <- breastcancer |>
  dplyr::mutate(
    continuous = 1:dplyr::n(),
    receptor = dplyr::if_else(dplyr::row_number() %in% 9:11, NA, receptor),
    death = dplyr::if_else(dplyr::row_number() %in% 99:101, NA, death),
    allempty = NA_integer_,
    allempty_lgl = allempty == 1)

result <- table1_design(
  death,
  receptor,
  dplyr::everything(),
  data = df)

test_that("Table 1 design dimensions are correct", {
  expect_equal(nrow(result), expected = 16)
  expect_true(all(names(result) %in% c("label", "outcome", "type", "na_rm")))
})

test_that("Table 1 design picks up missing values", {
  expect_true(all(
    c("death@_NA_", "receptor@_NA_", "allempty@_NA_", "allempty_lgl@_NA_") %in%
      result$outcome))
  expect_false(all(c("stage@_NA_", "continuous@_NA_") %in% result$outcome))
})

test_that("Table 1 design finds variable labels", {
  expect_equal(result$label[[2]], expected = "Death")
})
