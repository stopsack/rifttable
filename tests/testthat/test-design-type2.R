test_that(
  desc = "designs with type2 work",
  code = {
    data(breastcancer, package = "risks")

    design <- tibble::tibble(
      type = c("outcomes", "total"),
      type2 = c("risk", "risk (ci)")
    ) %>%
      dplyr::mutate(
        exposure = "stage",
        outcome = "death"
      )

    object <- rifttable(
      design = design,
      data = breastcancer
    )
    expected <- tibble::tribble(
      ~Stage,     ~`Stage I`,          ~`Stage II`,         ~`Stage III`,
      "outcomes", "7",                 "26",                "21",
      "",         "0.10",              "0.27",              "0.72",
      "total",    "67",                "96",                "29",
      "",         "0.10 (0.05, 0.20)", "0.27 (0.19, 0.37)", "0.72 (0.54, 0.85)"
    )
    expect_equal(
      object = object,
      expected = expected
    )

    object <- rifttable(
      design = design,
      data = breastcancer,
      layout = "cols",
    )
    expected <- tibble::tribble(
      ~Stage,       ~outcomes, ~total,
      "Stage I",    "7",       "67",
      "",           "0.10",    "0.10 (0.05, 0.20)",
      "Stage II",   "26",      "96",
      "",           "0.27",    "0.27 (0.19, 0.37)",
      "Stage III",  "21",      "29",
      "",           "0.72",    "0.72 (0.54, 0.85)"
    )
    expect_equal(
      object = object,
      expected = expected
    )

    object <- rifttable(
      design = design,
      data = breastcancer,
      layout = "cols",
      type2_layout = "cols",
    ) %>%
      dplyr::mutate(Stage = as.character(Stage))
    expected <- tibble::tribble(
      ~Stage,      ~outcomes, ~`outcomes `, ~total,  ~`total `,
      "Stage I",   "7",       "0.10",       "67",    "0.10 (0.05, 0.20)",
      "Stage II",  "26",      "0.27",       "96",    "0.27 (0.19, 0.37)",
      "Stage III", "21",      "0.72",       "29",    "0.72 (0.54, 0.85)"
    )
    expect_equal(
      object = object,
      expected = expected
    )

    object <- rifttable(
      design = design,
      data = breastcancer,
      layout = "rows",
      type2_layout = "cols",
    )
    expected <- tibble::tribble(
      ~Stage,    ~`Stage I`, ~`Stage I `,         ~`Stage II`, ~`Stage II `,        ~`Stage III`, ~`Stage III `,
      "outcomes", "7",       "0.10",              "26",        "0.27",              "21",         "0.72",
      "total",    "67",      "0.10 (0.05, 0.20)", "96",        "0.27 (0.19, 0.37)", "29",         "0.72 (0.54, 0.85)"
    )
    expect_equal(
      object = object,
      expected = expected
    )

    # Blanks in the label
    object <- design %>%
      dplyr::mutate(label = c("label", "")) %>%
      rifttable(
        data = breastcancer,
        layout = "cols"
      )
    expected <- tibble::tribble(
      ~Stage,      ~`1_label`, ~`2_`,
      "Stage I",   "7",        "67",
      "",          "0.10",     "0.10 (0.05, 0.20)",
      "Stage II",  "26",       "96",
      "",          "0.27",     "0.27 (0.19, 0.37)",
      "Stage III", "21",       "29",
      "",          "0.72",     "0.72 (0.54, 0.85)"
    )
    expect_equal(
      object = object,
      expected = expected
    )

    object <- design %>%
      dplyr::mutate(label = c("label", "")) %>%
      rifttable(
        data = breastcancer,
        layout = "cols",
        type2_layout = "cols"
      ) %>%
      dplyr::mutate(Stage = as.character(Stage))
    expected <- tibble::tribble(
      ~Stage,      ~`1_label`, ~`1_label `, ~`2_`,  ~`2_ `,
      "Stage I",   "7",        "0.10",       "67",   "0.10 (0.05, 0.20)",
      "Stage II",  "26",       "0.27",       "96",   "0.27 (0.19, 0.37)",
      "Stage III", "21",       "0.72",       "29",   "0.72 (0.54, 0.85)"
    )
    expect_equal(
      object = object,
      expected = expected
    )

    expect_error(
      object = design %>%
        dplyr::mutate(exposure = NA) %>%
        rifttable(data = breastcancer),
      regexp = "'exposure' must be specified for each row"
    )
  }
)
