data(breastcancer, package = "risks")

test_that(
  desc = "trend works for binary",
  code = {
    design <- tibble::tibble(
      type = "rr",
      exposure = "stage",
      outcome = "death",
      trend = "stage_num"
    )

    expect_equal(
      object = rifttable(
        design = design,
        data = breastcancer %>%
          dplyr::mutate(
            stage_num = as.numeric(stage)
          )
      ),
      expected = tibble::tibble(
        Stage = "rr",
        `Stage I` = "1 (reference)",
        `Stage II` = "2.59 (1.20, 5.6)",
        `Stage III` = "6.9 (3.3, 14)",
        Trend = "2.50 (1.97, 3.2)"
      )
    )
  }
)

test_that(
  desc = "trend works for continuous",
  code = {
    design <- tibble::tibble(
      type = "diff",
      exposure = "stage",
      outcome = "cont",
      trend = "stage_num"
    )

    expect_equal(
      object = rifttable(
        design = design,
        data = breastcancer %>%
          dplyr::mutate(
            stage_num = as.numeric(stage),
            cont = death * 10
          ),
        diff_digits = 0
      ),
      expected = tibble::tibble(
        Stage = "diff",
        `Stage I` = "0 (reference)",
        `Stage II` = "2 (0, 3)",
        `Stage III` = "6 (4, 8)",
        Trend = "3 (2, 4)"
      )
    )
  }
)

test_that(
  desc = "trend input errors are caught",
  code = {
    design <- tibble::tibble(
      type = "diff",
      outcome = "death",
      trend = "stage"
    )
    expect_error(
      object = rifttable(
        design = design,
        data = breastcancer
      ),
      regexp = "Trend variable 'stage' is not continuous"
    )

    expect_error(
      object = rifttable(
        design = design %>%
          dplyr::mutate(trend = "aaa"),
        data = breastcancer
      ),
      regexp = "Trend variable 'aaa' is not valid for the dataset."
    )
  }
)

test_that(
  desc = "trend is blank for type = 'blank'",
  code = {
    design <- tibble::tibble(
      type = "",
      exposure = "stage",
      trend = "death",
      outcome = "death"
    )
    expect_equal(
      object = rifttable(
        design = design,
        data = breastcancer
      ),
      expected = tibble::tibble(
        Stage = "",
        `Stage I` = "",
        `Stage II` = "",
        `Stage III` = "",
        Trend = ""
      )
    )
  }
)

test_that(
  desc = "confounders = NA is fine",
  code = {
    design <- tibble::tibble(
      type = "rr",
      exposure = "stage",
      outcome = "death",
      confounders = NA
    )
    expect_equal(
      object = rifttable(
        design = design,
        data = breastcancer
      ),
      expected = tibble::tribble(
        ~Stage, ~`Stage I`,      ~`Stage II`,        ~`Stage III`,
        "rr",   "1 (reference)", "2.59 (1.20, 5.6)", "6.9 (3.3, 14)"
      )
    )
  }
)


test_that(
  desc = "Non-comparative tables work",
  code = {
    design <- tibble::tibble(
      type = c("rr", "diff"),
      exposure = "receptor",
      outcome = "death"
    )

    expect_equal(
      object = rifttable(
        design = design,
        data = breastcancer %>%
          dplyr::filter(receptor == "Low")
      ),
      expect = tibble::tribble(
        ~`Hormone receptor`, ~Low,
        "rr",                "",
        "diff",              ""
      )
    )
  }
)

