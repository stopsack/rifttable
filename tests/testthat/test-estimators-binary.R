testthat::test_that(
  desc = "binary outcomes work",
  code = {
    data(breastcancer, package = "risks")

    object <- tibble::tribble(
      ~label,                      ~type,
      "**Absolute estimates**",    "",
      "Observations",              "total",
      "Outcomes",                  "outcomes",
      "Outcomes/Total",            "outcomes/total",
      "Cases/Controls",            "cases/controls",
      "Risk",                      "risk",
      "Risk (95% CI)",             "risk (ci)",
      "Outcomes (Risk)",           "outcomes (risk)",
      "Outcomes/Total (Risk)",     "outcomes/total (risk)",
      "",                          "",
      "**Comparative estimates**", "",
      "Risk ratio (95% CI)",       "rr",
      "Risk difference (95% CI)",  "rd",
      "Odds ratio (95% CI)",       "or"
    ) |>
      dplyr::mutate(
        exposure = "stage",
        outcome = "death"
      ) |>
      rifttable(
        data = breastcancer,
        overall = TRUE
      )

    expected <- tibble::tribble(
      ~Summary,                   ~Overall,           ~`Stage I`,         ~`Stage II`,        ~`Stage III`,
      "**Absolute estimates**",   "",                 "",                 "",                 "",
      "Observations",             "192",              "67",               "96",               "29",
      "Outcomes",                 "54",               "7",                "26",               "21",
      "Outcomes/Total",           "54/192",           "7/67",             "26/96",            "21/29",
      "Cases/Controls",           "54/138",           "7/60",             "26/70",            "21/8",
      "Risk",                     "0.28",             "0.10",             "0.27",             "0.72",
      "Risk (95% CI)",            "0.28 (0.22, 0.35)","0.10 (0.05, 0.20)","0.27 (0.19, 0.37)","0.72 (0.54, 0.85)",
      "Outcomes (Risk)",          "54 (0.28)",        "7 (0.10)",         "26 (0.27)",        "21 (0.72)",
      "Outcomes/Total (Risk)",    "54/192 (0.28)",    "7/67 (0.10)",      "26/96 (0.27)",     "21/29 (0.72)",
      "",                         "",                 "",                 "",                 "",
      "**Comparative estimates**","",                 "",                 "",                 "",
      "Risk ratio (95% CI)",      "",                 "1 (reference)",    "2.59 (1.20, 5.6)", "6.9 (3.3, 14)",
      "Risk difference (95% CI)", "",                 "0 (reference)",    "0.17 (0.05, 0.28)","0.62 (0.44, 0.80)",
      "Odds ratio (95% CI)",      "",                 "1 (reference)",    "3.2 (1.35, 8.4)",  "22 (7.7, 75)"
    )

    expect_equal(
      object = object,
      expected = expected
    )
  }
)
