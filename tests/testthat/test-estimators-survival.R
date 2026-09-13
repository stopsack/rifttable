testthat::test_that(
  desc = "survival outcomes work",
  code = {
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
        time = time / 365.25, # transform to years
        status = status - 1
      )

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
      "  Observations, *N*",                  "228",                  "138",                   "90",
      "  Events, *n*",                        "165",                  "112",                   "53",
      "  Events/observations",                "165/228",              "112/138",               "53/90",
      "  Events/person-years",                "165/191",              "112/107",               "53/84",
      "*Follow-up*",                          "",                     "",                      "",
      "  Person-years",                       "191",                  "107",                   "84",
      "  Maximum follow-up, years",           "2.80",                 "2.80",                  "2.64",
      "  Median follow-up, years",            "1.61",                 "2.30",                  "1.45",
      "  Median follow-up (IQR), years",      "1.61 (0.82, 2.64)",    "2.30 (1.11, 2.77)",     "1.45 (0.76, 2.25)",
      "*Rates*",                              "",                     "",                      "",
      "  Rate per 1000 person-years",         "866.0",                "1046.6",                "634.6",
      "  Rate per 1000 person-years (95% CI)","866.0 (743.4, 1008.7)","1046.6 (869.7, 1259.6)","634.6 (484.8, 830.6)",
      "  Events/py (rate per 1000 py)",       "165/191 (866.0)",      "112/107 (1046.6)",      "53/84 (634.6)",
      "*Risks*",                              "",                     "",                      "",
      "  1-year survival",                    "0.41",                 "0.34",                  "0.53",
      "  1-year survival (95% CI)",           "0.41 (0.34, 0.49)",    "0.34 (0.26, 0.43)",     "0.53 (0.42, 0.66)",
      "  1-year risk/cumulative incidence",   "0.59",                 "0.66",                  "0.47",
      "  1-year risk (95% CI)",               "0.59 (0.51, 0.66)",    "0.66 (0.57, 0.74)",     "0.47 (0.34, 0.58)",
      "  Median survival, years",             "0.85",                 "0.74",                  "1.17",
      "  Median survival (95 CI), years",     "0.85 (0.78, 0.99)",    "0.74 (0.58, 0.85)",     "1.17 (0.95, 1.51)",
      "",                                     "",                     "",                      "",
      "**Comparative estimates**",            "",                     "",                      "",
      "  1-year survival difference",         "",                     "0 (reference)",         "0.19 (0.05, 0.34)",
      "  1-year risk difference",             "",                     "0 (reference)",         "-0.19 (-0.34, -0.05)",
      "  1-year survival ratio",              "",                     "1 (reference)",         "1.57 (1.12, 2.19)",
      "  1-year risk ratio",                  "",                     "1 (reference)",         "0.71 (0.51, 0.92)",
      "  Hazard ratio (95% CI)",              "",                     "1 (reference)",         "0.59 (0.42, 0.82)"
    )

    expect_equal(
      object = object,
      expected = expected
    )

    expect_equal(
      object = tibble::tibble(
        type = c("hr", "cuminc"),
        time = "time",
        event = "status",
        exposure = "ph.ecog",
        trend = "ph.ecog_num",
        weights = "w"
      ) |>
        rifttable(
          data = cancer |>
            dplyr::filter(ph.ecog < 3) |>
            dplyr::mutate(
              w = 1 / age,
              ph.ecog = factor(ph.ecog),
              ph.ecog_num = as.numeric(ph.ecog)
            )
        ),
      expected = tibble::tribble(
        ~ph.ecog, ~`0`, ~`1`, ~`2`, ~Trend,
        "hr", "1 (reference)", "1.53 (1.04, 2.24)", "2.69 (1.71, 4.2)", "1.65 (1.31, 2.07)",
        "cuminc", "0.92", "0.93", "1.00", ""
      )
    )
  }
)


testthat::test_that(
  desc = "cumulative incidence limits mirror the survival limits",
  code = {
    data(cancer, package = "survival")

    cancer <- cancer |>
      tibble::as_tibble() |>
      dplyr::mutate(
        sex = factor(
          sex,
          levels = 1:2,
          labels = c(
            "Male",
            "Female"
          )
        ),
        status = status - 1
      )

    formula <- survival::Surv(time = time, event = status) ~ sex

    surv <- survdiff_ci(
      formula = formula,
      data = cancer,
      time = 365.25,
      estimand = "survival"
    )
    cuminc <- survdiff_ci(
      formula = formula,
      data = cancer,
      time = 365.25,
      estimand = "cuminc"
    )

    # The difference in 1 - S(t) is the negative of the difference in S(t),
    # so its limits are the negated limits of the survival difference,
    # reversed.
    expect_equal(
      object = cuminc$estimate,
      expected = -surv$estimate
    )
    expect_equal(
      object = cuminc$conf.low,
      expected = -surv$conf.high
    )
    expect_equal(
      object = cuminc$conf.high,
      expected = -surv$conf.low
    )

    # The ratio by hand: MOVER on the log scale, with the lower limit of
    # 1 - S(t) being 1 minus the upper limit of S(t), and vice versa.
    fit <- summary(
      survival::survfit(formula = formula, data = cancer),
      times = 365.25,
      extend = TRUE
    )
    ci <- 1 - fit$surv
    lower <- 1 - fit$upper
    upper <- 1 - fit$lower
    estimate <- log(ci[2]) - log(ci[1])

    ratio <- survdiff_ci(
      formula = formula,
      data = cancer,
      time = 365.25,
      estimand = "cuminc",
      type = "ratio"
    )

    expect_equal(
      object = ratio$estimate,
      expected = exp(estimate)
    )
    expect_equal(
      object = ratio$conf.low,
      expected = exp(
        estimate -
          sqrt((log(ci[2]) - log(lower[2]))^2 + (log(upper[1]) - log(ci[1]))^2)
      )
    )
    expect_equal(
      object = ratio$conf.high,
      expected = exp(
        estimate +
          sqrt((log(upper[2]) - log(ci[2]))^2 + (log(ci[1]) - log(lower[1]))^2)
      )
    )
  }
)
