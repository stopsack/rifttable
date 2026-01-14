data("breastcancer", package = "risks")
df <- breastcancer |>
  dplyr::mutate(
    continuous = 1:dplyr::n(),
    receptor = {
      label <- attr(receptor, "label")
      receptor <- dplyr::if_else(dplyr::row_number() %in% 9:11, NA, receptor)
      attr(receptor, "label") <- label
      receptor
    },
    death = {
      label <- attr(death, "label")
      dplyr::if_else(dplyr::row_number() %in% 99:101, NA, death)
      attr(death, "label") <- label
      death
    },
    allempty = NA_integer_,
    allempty_lgl = allempty == 1
  )

result <- table1_design(
  death,
  receptor,
  dplyr::everything(),
  data = df
)

test_that("Table 1 design dimensions are correct", {
  expect_equal(nrow(result), expected = 16)
  expect_true(all(names(result) %in% c("label", "outcome", "type", "na_rm")))
})

test_that("Table 1 design picks up missing values", {
  expect_true(all(
    c("death@_NA_", "receptor@_NA_", "allempty@_NA_", "allempty_lgl@_NA_") %in%
      result$outcome
  ))
  expect_false(all(c("stage@_NA_", "continuous@_NA_") %in% result$outcome))
})

test_that("Table 1 design finds variable labels", {
  expect_equal(result$label[[2]], expected = "Death")
})

test_that(
  desc = "Table 1 gets generated",
  code = {
    object <- rifttable(
      design = result,
      data = df
    )
    expected <- tibble::tribble(
      ~Summary,           ~Overall,
      "N",                "192",
      "Death",            "54 (29%)",
      "  Unknown",        "3",
      "Hormone receptor", "",
      "  High",           "144 (76%)",
      "  Low",            "45 (24%)",
      "  Unknown",        "3",
      "Stage",            "",
      "  Stage I",        "67 (35%)",
      "  Stage II",       "96 (50%)",
      "  Stage III",      "29 (15%)",
      "continuous",       "96.50 (48.75, 144.25)",
      "allempty",         "",
      "  Unknown",        "192",
      "allempty_lgl",     "",
      "  Unknown",        "192"
    )
    expect_equal(
      object = object,
      expected = expected
    )
  }
)

test_that(
  desc = "Table 1: by = works",
  code = {
    object <- table1_design(
      continuous,
      by = receptor,
      data = df
    )
    attr(x = object, which = "rt_data") <- NULL
    expect_equal(
      object = object,
      expected = tibble::tribble(
        ~label,       ~outcome,     ~type,          ~exposure,
        "N",          "",           "total",        "receptor",
        "continuous", "continuous", "median (iqr)", "receptor"
      )
    )
  }
)


test_that(
  desc = "Table 1 works without variables",
  code = {
    expect_equal(
      object = nrow(table1_design(data = df)),
      expected = 16
    )
  }
)

test_that(
  desc = "Table 1 with empty levels",
  code = {
    levels(df$stage) <- c(levels(df$stage), "Stage IV")
    object <- table1_design(
      stage,
      by = receptor,
      data = df,
      empty_levels = TRUE
    )
    expect_equal(
      object = nrow(object),
      expected = 6
    )
  }
)
