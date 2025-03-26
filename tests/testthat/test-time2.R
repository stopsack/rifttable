data(cancer, package = "survival")

cancer <- cancer |>
  tibble::as_tibble() |>
  dplyr::mutate(
    # The exposure (here, 'sex') must be categorical (a factor)
    sex = factor(
      sex,
      levels = 1:2,
      labels = c(
        "Male",
        "Female"
      )
    ),
    time2 = time / 365.25, # transform to years
    time = 0.1,
    status = status - 1
  ) |>
  dplyr::filter(time2 > time)

testthat::test_that(
  desc = "time2 works",
  code = {
    object <- tibble::tribble(
      ~label,                                  ~type,
      "**Absolute estimates**",                "",
      "*Counts and sums*",                     "",
      "  Observations, *N*",                   "total",
      "  Events, *n*",                         "events",
      "  Events/observations",                 "events/total",
      "  Events/person-years",                 "events/time",
      "*Follow-up*",                           "",
      "  Person-years",                        "time",
      "  Maximum follow-up, years",            "maxfu",
      "  Median follow-up, years",             "medfu",
      "  Median follow-up (IQR), years",       "medfu (iqr)",
      "*Rates*",                               "",
      "  Rate per 1000 person-years",          "rate",
      "  Rate per 1000 person-years (95% CI)", "rate (ci)",
      "  Events/py (rate per 1000 py)",        "events/time (rate)",
      "*Risks*",                               "",
      "  1-year survival",                     "surv",
      "  1-year survival (95% CI)",            "surv (ci)",
      "  1-year risk/cumulative incidence",    "cuminc",
      "  1-year risk (95% CI)",                "cuminc (ci)",
      "  Median survival, years",              "medsurv",
      "  Median survival (95 CI), years",      "medsurv (ci)",
      "",                                      "",
      "**Comparative estimates**",             "",
      "  1-year survival difference",          "survdiff",
      "  1-year risk difference",              "cumincdiff",
      "  1-year survival ratio",               "survratio",
      "  1-year risk ratio",                   "cumincratio",
      "  Hazard ratio (95% CI)",               "hr"
    ) |>
      dplyr::mutate(
        time = "time",
        time2 = "time2",
        event = "status",
        exposure = "sex",
        arguments = list(list(timepoint = 1))
      ) |>
      rifttable(
        data = cancer,
        overall = TRUE
      )

    expected <- tibble::tribble(
      ~Summary,                               ~Overall,               ~Male,                   ~Female,
      "**Absolute estimates**",               "",                     "",                      "",
      "*Counts and sums*",                    "",                     "",                      "",
      "  Observations, *N*",                  "217",                  "128",                   "89",
      "  Events, *n*",                        "154",                  "102",                   "52",
      "  Events/observations",                "154/217",              "102/128",               "52/89",
      "  Events/person-years",                "154/168",              "102/94",                "52/75",
      "*Follow-up*",                          "",                     "",                      "",
      "  Person-years",                       "168",                  "94",                    "75",
      "  Maximum follow-up, years",           "2.70",                 "2.70",                  "2.54",
      "  Median follow-up, years",            "1.61",                 "2.30",                  "1.45",
      "  Median follow-up (IQR), years",      "1.61 (0.82, 2.64)",    "2.30 (1.11, 2.77)",     "1.45 (0.76, 2.25)",
      "*Rates*",                              "",                     "",                      "",
      "  Rate per 1000 person-years",         "914.8",                "1088.1",                "697.0",
      "  Rate per 1000 person-years (95% CI)","914.8 (781.1, 1071.3)","1088.1 (896.2, 1321.2)","697.0 (531.1, 914.6)",
      "  Events/py (rate per 1000 py)",       "154/168 (914.8)",      "102/94 (1088.1)",       "52/75 (697.0)",
      "*Risks*",                              "",                     "",                      "",
      "  1-year survival",                    "0.43",                 "0.36",                  "0.53",
      "  1-year survival (95% CI)",           "0.43 (0.36, 0.51)",    "0.36 (0.28, 0.46)",     "0.53 (0.43, 0.66)",
      "  1-year risk/cumulative incidence",   "0.57",                 "0.64",                  "0.47",
      "  1-year risk (95% CI)",               "0.57 (0.49, 0.64)",    "0.64 (0.54, 0.72)",     "0.47 (0.34, 0.57)",
      "  Median survival, years",             "0.93",                 "0.78",                  "1.17",
      "  Median survival (95 CI), years",     "0.93 (0.80, 1.02)",    "0.78 (0.63, 0.97)",     "1.17 (0.95, 1.51)",
      "",                                     "",                     "",                      "",
      "**Comparative estimates**",            "",                     "",                      "",
      "  1-year survival difference",         "",                     "0 (reference)",         "0.17 (0.02, 0.32)",
      "  1-year risk difference",             "",                     "0 (reference)",         "-0.17 (-0.32, -0.02)",
      "  1-year survival ratio",              "",                     "1 (reference)",         "1.47 (1.05, 2.05)",
      "  1-year risk ratio",                  "",                     "1 (reference)",         "0.73 (0.56, 1.04)",
      "  Hazard ratio (95% CI)",              "",                     "1 (reference)",         "0.63 (0.45, 0.87)"
    )

    expect_equal(
      object = object,
      expected = expected
    )
  }
)

testthat::test_that(
  desc = "catch time2 containing NA",
  code = {
    expect_warning(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          time = "time",
          time2 = "time2",
          event = "status"
        ),
        data = cancer |>
          dplyr::mutate(
            time2 = dplyr::if_else(
              dplyr::row_number() < 5,
              true = NA_real_,
              false = time2
            )
          )
      ),
      regexp = "'time2' contains missing values"
    )
  }
)

testthat::test_that(
  desc = "catch time2 of wrong type",
  code = {
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          time = "time",
          time2 = "sex",
          event = "status"
        ),
        data = cancer
      ),
      regexp = "time2 variable 'sex' must be continuous"
    )
  }
)

testthat::test_that(
  desc = "catch wrong time2 variable",
  code = {
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "rate",
          time = "time",
          time2 = "aaa",
          event = "status"
        ),
        data = cancer
      ),
      regexp = "time2 variable 'aaa' is not valid"
    )
  }
)


testthat::test_that(
  desc = "squareadd risk difference works",
  code = {
    result <- survdiff_ci(
      formula = survival::Surv(
        time = time,
        event = status
      ) ~
        sex,
      data = cancer,
      time = 365.25,
      approach = "squareadd"
    )

    expect_equal(
      object = result$term,
      expected = "Female"
    )
    expect_equal(
      object = result$estimate,
      expected = 0.21260534
    )
    expect_equal(
      object = result$conf.low,
      expected = 0.0887429
    )
    expect_equal(
      object = result$conf.high,
      expected = 0.33646778
    )
  }
)


testthat::test_that(
  desc = "Invalid ID variable is found",
  code = {
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "surv",
          time = "time",
          event = "status",
          exposure = "sex",
        ),
        data = cancer,
        id = "nonsense"
      ),
      regexp = "is not an ID variable that is valid"
    )
  }
)

testthat::test_that(
  desc = "ID variable gets used",
  code = {
    expect_equal(
      object = rifttable(
        design = tibble::tibble(
          type = "surv (ci)",
          time = "time",
          time2 = "time2",
          event = "status",
          arguments = list(list(timepoint = 1))
        ),
        data = cancer |>
          dplyr::mutate(
            idvar = rep(
              1:50,
              length.out = nrow(cancer)
            )
          ),
        id = "idvar"
      ),
      expected = tibble::tibble(
        Summary = "surv (ci)",
        Overall = "0.43 (0.37, 0.50)"
      )
    )
  }
)

testthat::test_that(
  desc = "old options are not used",
  code = {
    expect_error(
      object = rifttable(
        design = tibble::tibble(
          type = "hr",
          time = "time",
          event = "status",
          exposure = "sex",
          arguments = list(list(weights = "aaa"))
        ),
        data = cancer
      ),
      regexp = "Breaking change in rifttable"
    )
  }
)

testthat::test_that(
  desc = "Cox model does not return results without exposure",
  code = {
    expect_equal(
      object = rifttable(
        design = tibble::tibble(
          type = "hr",
          time = "time",
          event = "status",
        ),
        data = cancer
      ),
      expected = tibble::tibble(
        Summary = "hr",
        Overall = ""
      )
    )
  }
)


testthat::test_that(
  desc = "Cox model does not return results with one exposure group",
  code = {
    expect_equal(
      object = rifttable(
        design = tibble::tibble(
          type = "hr",
          time = "time",
          event = "status",
          exposure = "sex"
        ),
        data = cancer |>
          dplyr::filter(sex == "Male")
      ),
      expected = tibble::tibble(
        sex = "hr",
        Male = ""
      )
    )
  }
)
