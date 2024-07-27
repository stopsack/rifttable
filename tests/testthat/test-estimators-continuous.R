testthat::test_that(
  desc = "continuous outcomes work",
  code = {
    data(cancer, package = "survival")

    cancer <- cancer |>
      dplyr::filter(ph.ecog < 3) |>
      dplyr::mutate(ph.ecog = factor(ph.ecog))
    attr(cancer$ph.ecog, which = "label") <- "ECOG performance status"

    object <- tibble::tribble(
      ~label,                                ~type,
      "**Absolute estimates**",              "",
      "Observations",                        "total",
      "Range",                               "range",
      "Mean",                                "",
      "  Mean (i.e., arithmetic mean)",      "mean",
      "  Mean (95% CI)",                     "mean (ci)",
      "  Mean (standard deviation)",         "mean (sd)",
      "  Geometric mean",                    "geomean",
      "Standard deviation",                  "sd",
      "Median",                              "median",
      "Median (interquartile range)",        "median (iqr)",
      "",                                    "",
      "**Comparative estimates**",           "",
      "Mean difference (95% CI)",            "diff",
      "Median difference (95% CI)",          "quantreg",
      "Mean ratio",                          "",
      "  of arithmetic means",               "fold",
      "  of arithmetic means, empirical SE", "irrrob",
      "  of arithmetic means, Poisson SE",   "irr",
      "  of geometric means",                "foldlog"
    ) |>
      dplyr::mutate(
        exposure = "ph.ecog",
        outcome = "age"
      ) |>
      rifttable(
        data = cancer,
        diff_digits = 1,
        ratio_digits = 3,
        overall = TRUE
      )

    expected <- tibble::tribble(
      ~Summary,                               ~Overall,         ~`0`,               ~`1`,                  ~`2`,
      "**Absolute estimates**",             "",                 "",                 "",                    "",
      "Observations",                       "226",              "63",               "113",                 "50",
      "Range",                              "39.0, 82.0",       "39.0, 82.0",       "40.0, 80.0",          "48.0, 77.0",
      "Mean",                               "",                 "",                 "",                    "",
      "  Mean (i.e., arithmetic mean)",     "62.4",             "61.2",             "61.5",                "66.2",
      "  Mean (95% CI)",                    "62.4 (61.2, 63.6)","61.2 (58.8, 63.5)","61.5 (59.8, 63.1)",   "66.2 (64.0, 68.5)",
      "  Mean (standard deviation)",        "62.4 (9.1)",       "61.2 (9.6)",       "61.5 (8.9)",          "66.2 (8.1)",
      "  Geometric mean",                   "61.7",             "60.4",             "60.8",                "65.7",
      "Standard deviation",                 "9.1",              "9.6",              "8.9",                "8.1",

      "Median",                             "63.0",             "61.0",             "63.0",                "68.0",
      "Median (interquartile range)",       "63.0 (56.0, 69.0)","61.0 (56.5, 68.0)","63.0 (55.0, 68.0)",   "68.0 (60.2, 73.0)",
      "",                                   "",                 "",                 "",                    "",
      "**Comparative estimates**",          "",                 "",                 "",                    "",
      "Mean difference (95% CI)",           "",                 "0 (reference)",    "0.3 (-2.5, 3.1)",     "5.1 (1.7, 8.4)",
      "Median difference (95% CI)",         "",                 "0 (reference)",    "2.0 (-4.8, 8.8)",     "7.0 (1.3, 12.2)",
      "Mean ratio",                         "",                 "",                 "",                    "",
      "  of arithmetic means",              "",                 "1 (reference)",    "1.005 (0.961, 1.051)","1.083 (1.028, 1.140)",
      "  of arithmetic means, empirical SE","",                 "1 (reference)",    "1.005 (0.959, 1.053)","1.083 (1.029, 1.139)",
      "  of arithmetic means, Poisson SE",  "",                 "1 (reference)",    "1.005 (0.966, 1.045)", "1.083 (1.034, 1.134)",
      "  of geometric means",               "",                 "1 (reference)",    "1.007 (0.961, 1.055)","1.088 (1.029, 1.151)",
    )

    expect_equal(
      object = object,
      expected = expected
    )
  }
)
