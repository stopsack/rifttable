data(cancer, package = "survival")

cancer <- cancer %>%
  tibble::as_tibble() %>%
  dplyr::filter(ph.ecog < 3) %>%
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
    time = time / 365.25,  # transform to years
    status = factor(
      ph.ecog,
      levels = 0:2,
      labels = c("Censor", "Event of interest", "Other event")
    )
  )

test_that(
  desc = "Competing event models work",
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
     # "  1-year survival",                     "surv",
    #  "  1-year survival (95% CI)",            "surv (ci)",
      "  1-year risk/cumulative incidence",    "cuminc",
      "  1-year risk (95% CI)",                "cuminc (ci)",
    #  "  Median survival, years",              "medsurv",
    #  "  Median survival (95 CI), years",      "medsurv (ci)",
      "",                                      "",
      "**Comparative estimates**",             "",
    #  "  1-year survival difference",          "survdiff",
      "  1-year risk difference",              "cumincdiff",
     # "  1-year survival ratio",               "survratio",
      "  1-year risk ratio",                   "cumincratio",
      "  Hazard ratio (95% CI)",               "hr"
    ) %>%
      dplyr::mutate(
        time = "time",
        event = "status@Event of interest",
        exposure = "sex",
        arguments = list(list(timepoint = 1))
      ) %>%
      rifttable(
        data = cancer,
        overall = TRUE
      )

    expected <- tibble::tribble(
      ~Summary,                               ~Overall,              ~Male,                 ~Female,
      "**Absolute estimates**",               "",                    "",                    "",
      "*Counts and sums*",                    "",                    "",                    "",
      "  Observations, *N*",                  "226",                 "136",                 "90",
      "  Events, *n*",                        "113",                 "71",                  "42",
      "  Events/observations",                "113/226",             "71/136",              "42/90",
      "  Events/person-years",                "113/190",             "71/106",              "42/84",
      "*Follow-up*",                          "",                    "",                    "",
      "  Person-years",                       "190",                 "106",                 "84",
      "  Maximum follow-up, years",           "2.80",                "2.80",                "2.64",
      "  Median follow-up, years",            "1.06",                "1.08",                "1.05",
      "  Median follow-up (IQR), years",      "1.06 (0.65, 1.94)",   "1.08 (0.62, 1.94)",   "1.05 (0.65, 1.93)",
      "*Rates*",                              "",                    "",                    "",
      "  Rate per 1000 person-years",         "594.7",               "666.7",               "502.9",
      "  Rate per 1000 person-years (95% CI)","594.7 (494.5, 715.1)","666.7 (528.3, 841.3)","502.9 (371.6, 680.4)",
      "  Events/py (rate per 1000 py)",       "113/190 (594.7)",     "71/106 (666.7)",      "42/84 (502.9)",
      "*Risks*",                              "",                    "",                    "",
      "  1-year risk/cumulative incidence",   "0.39",                "0.45",                "0.31",
      "  1-year risk (95% CI)",               "0.39 (0.33, 0.47)",   "0.45 (0.36, 0.54)",   "0.31 (0.22, 0.43)",
      "",                                     "",                    "",                    "",
      "**Comparative estimates**",            "",                    "",                    "",
      "  1-year risk difference",             "",                    "0 (reference)",       "-0.14 (-0.27, 0.01)",
      "  1-year risk ratio",                  "",                    "1 (reference)",       "0.69 (0.47, 1.03)",
      "  Hazard ratio (95% CI)",              "",                    "1 (reference)",       "0.73 (0.50, 1.07)"
    )

    expect_equal(
      object = object,
      expected = expected
    )
  }
)


test_that(
  desc = "Survival is not calculated for competing events",
  code = {
    expect_error(
      object = tibble::tibble(
        type = "surv (ci)",
        time = "time",
        event = "status@Event of interest",
        exposure = "sex"
      ) %>%
        rifttable(
          data = cancer,
          overall = TRUE
        ),
      regexp = "Survival \\(type = 'surv'\\) is not estimated with competing risks"
    )

    expect_warning(
      object = tibble::tibble(
        type = "medsurv",
        time = "time",
        event = "status@Event of interest",
        exposure = "sex"
      ) %>%
        rifttable(data = cancer),
      regexp = "Note the presence of competing events"
    )

    expect_error(
      object = tibble::tribble(
        ~label,                                  ~type,
        "1-year survival difference",          "survdiff",
        "1-year survival ratio",               "survratio"
      ) %>%
        dplyr::mutate(
          time = "time",
          event = "status@Event of interest",
          exposure = "sex",
          arguments = list(list(timepoint = 1))
        ) %>%
        rifttable(
          data = cancer,
          overall = TRUE
        ),
      regexp = "may not be meaningful with competing events"
    )
  }
)

test_that(
  desc = "Missing time horizon is caught",
  code = {
    expect_error(
      object = tibble::tibble(
        type = "survdiff",
          time = "time",
          event = "status@Event of interest",
          exposure = "sex"
        ) %>%
        rifttable(
          data = cancer,
          overall = TRUE
        ),
      regexp = "Must provide a time horizon for survival analysis of type"
    )
  }
)

test_that(
  desc = "Wrong event types are caught",
  code = {
    expect_error(
      object = tibble::tibble(
        type = "survdiff",
        time = "time",
        event = "status@Nonsense",
        exposure = "sex"
      ) %>%
        rifttable(
          data = cancer,
          overall = TRUE
        ),
      regexp = "event variable 'status', the specified event type 'Nonsense' is not available"
    )
  }
)

test_that(
  desc = "Non-competing events setting is identified",
  code = {
    expect_error(
      object = tibble::tibble(
        type = "survdiff",
        time = "time",
        event = "sex@Male",
        exposure = "status"
      ) %>%
        rifttable(
          data = cancer,
          overall = TRUE
        ),
      regexp = "event variable does not appear to have more than two levels"
    )
  }
)

test_that(
  desc = "Missing event type is found",
  code = {
    expect_error(
      object = tibble::tibble(
        type = "survdiff",
        time = "time",
        event = "status",
        exposure = "sex"
      ) %>%
        rifttable(
          data = cancer %>%
            dplyr::filter(!is.na(ph.ecog)),
          overall = TRUE
        ),
      regexp = "competing events may be encoded, but no specific event type"
    )
  }
)

test_that(
  desc = "Wrong event variable class is found",
  code = {
    expect_error(
      object = tibble::tibble(
        type = "survdiff",
        time = "time",
        event = "pat.karno@50",
        exposure = "sex"
      ) %>%
        rifttable(
          data = cancer %>%
            dplyr::filter(!is.na(pat.karno)),
          overall = TRUE
        ),
      regexp = "to presumably encode competing events must be a factor"
    )
  }
)
