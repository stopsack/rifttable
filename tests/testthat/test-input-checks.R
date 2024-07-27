test_that(
  desc = "Basic inputs are present",
  code = {
    expect_error(
      object = rifttable(
        design = 1,
        data = tibble::tibble()
      ),
      regexp = "No 'design' data frame"
    )

    expect_error(
      object = rifttable(
        design = tibble::tibble(),
        data = 1
      ),
      regexp = "No 'data' data frame"
    )

    design <- tibble::tibble()
    attr(x = design, which = "rt_data") <- tibble::tibble()
    expect_error(
      object = rifttable(
        design = design
      ),
      regexp = "The 'design' data frame must contain a 'type' column"
    )
  }
)

test_that(
  desc = "Invalid design variables get caught",
  code = {
    data(breastcancer, package = "risks")

    design <- tibble::tibble(
      stratum = c("Stage I", "Stage II")
    ) %>%
      dplyr::mutate(
        type = "risk",
        exposure = "receptor",
        outcome = "death",
        effect_modifier = "stage"
      )

    expect_warning(
      object = rifttable(
        design = design,
        data = breastcancer %>%
          dplyr::filter(stage != "Stage I")
      ),
      regexp = "Stratum 'Stage I' is empty"
    )

    expect_error(
      object = rifttable(
        design = design %>%
          dplyr::mutate(outcome = "d"),
        data = breastcancer
      ),
      regexp = "Outcome variable 'd' is not valid"
    )

    expect_error(
      object = rifttable(
        design = design %>%
          dplyr::mutate(weights = "w"),
        data = breastcancer
      ),
      regexp = "Variable is not valid"
    )

    expect_error(
      object = rifttable(
        design = design %>%
          dplyr::mutate(weights = "stage"),
        data = breastcancer
      ),
      regexp = "'stage': Variable is not numeric"
    )

    expect_warning(
      object = rifttable(
        design = design %>%
          dplyr::mutate(exposure = "row"),
        data = breastcancer |>
          dplyr::mutate(row = dplyr::row_number())
      ),
      regexp = "Exposure variable 'row' is not categorical"
    )

    expect_error(
      object = rifttable(
        design = design %>%
          dplyr::mutate(type = "nonsense"),
        data = breastcancer
      ),
      regexp = "An estimator type = 'nonsense' is not implemented by default"
    )

    expect_error(
      object = rifttable(
        design = design %>%
          dplyr::mutate(type = "regress_binary"),
        data = breastcancer
      ),
      regexp = "Invalid estimator type = 'regress_binary'."
    )
  }
)

testthat::test_that(
  desc = "catch time containing NA",
  code = {
    expect_warning(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          time = "death",
          event = "death"
        ),
        data = breastcancer %>%
          dplyr::mutate(
            death = dplyr::if_else(
              dplyr::row_number() < 5,
              true = NA_real_,
              false = death
            )
          )
      ),
      regexp = "'death' and/or the time variable 'death' contain missing values"
    )
  }
)


testthat::test_that(
  desc = "catch time of wrong type",
  code = {
    data(cancer, package = "survival")
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          time = "stage",
          event = "death"
        ),
        data = breastcancer
      ),
      regexp = "Time variable 'stage' must be continuous"
    )
  }
)

testthat::test_that(
  desc = "catch wrong time variable",
  code = {
    data(cancer, package = "survival")
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          time = "aaa",
          event = "death"
        ),
        data = breastcancer
      ),
      regexp = "Time variable 'aaa' is not valid"
    )
  }
)


test_that(
  desc = "Rounding digits are valid",
  code = {
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "total",
          digits = 11
        ),
        data = tibble::tibble()
      ),
      regexp = "must be an integer number from 0 to 10"
    )
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "total",
          digits = "a"
        ),
        data = tibble::tibble()
      ),
      regexp = "must be numeric. 'a' is not numeric."
    )
  }
)

testthat::test_that(
  desc = "event variable is valid",
  code = {
    data(breastcancer, package = "risks")
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          time = "death",
          event = "aaa"
        ),
        data = cancer
      ),
      regexp = "Event variable 'aaa' is not valid for the dataset"
    )
  }
)

testthat::test_that(
  desc = "time and event variable are present if needed",
  code = {
    data(breastcancer, package = "risks")
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          time = "death",
        ),
        data = cancer
      ),
      regexp = " The 'design' must contain 'event' and 'time' variables"
    )
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          event = "death",
        ),
        data = cancer
      ),
      regexp = " The 'design' must contain 'event' and 'time' variables"
    )
  }
)

testthat::test_that(
  desc = "extra arguments get checked",
  code = {
    data(breastcancer, package = "risks")
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "cuminc",
          time = "death",
          event = "death",
          arguments = list(list(timepoint = "a"))
        ),
        data = breastcancer
      ),
      regexp = "A timepoint argument was supplied, but timepoint = 'a' is not numeric."
    )
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rr",
          exposure = "receptor",
          outcome = "death",
          arguments = list(list(approach = "a"))
        ),
        data = breastcancer
      ),
      regexp = "approach = 'a' is not among the accepted choices, which include: auto"
    )
  }
)

testthat::test_that(
  desc = "ratio digits decrease errors are found",
  code = {
    data(breastcancer, package = "risks")
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rr",
          exposure = "receptor",
          outcome = "death",
          ratio_digits_decrease = c(a = 1)
        ),
        data = breastcancer
      ),
      regexp = "Names of 'ratio_digits_decrease' for rounding, if provided, must be convertible into numbers"
    )
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rr",
          exposure = "receptor",
          outcome = "death",
          ratio_digits_decrease = c(`-1` = "a")
        ),
        data = breastcancer
      ),
      regexp = "Values of 'ratio_digits_decrease' for rounding, if provided, must be numeric"
    )
  }
)

testthat::test_that(
  desc = "scoreci midpoint",
  code = {
    expect_contains(
      object = names(
        scoreci(
          success = 5,
          total = 10,
          return_midpoint = TRUE
        )
      ),
      expected = "midpoint"
    )
  }
)
