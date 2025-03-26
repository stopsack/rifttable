test_that(
  desc = "stratified and joint models work",
  code = {
    data(breastcancer, package = "risks")

    design <- tibble::tibble(
      stratum = c("Low", "High"),
      label = stratum,
      type = "rr",
      exposure = "stage",
      outcome = "death",
      effect_modifier = "receptor"
    )

    object <- rifttable(
      design = design,
      data = breastcancer
    )
    expected <- tibble::tribble(
      ~Stage, ~`Stage I`,      ~`Stage II`,        ~`Stage III`,
      "Low",  "1 (reference)", "2.45 (0.63, 9.6)", "5.1 (1.43, 19)",
      "High", "1 (reference)", "2.53 (0.99, 6.4)", "6.6 (2.60, 17)"
    )
    expect_equal(
      object = object,
      expected = expected
    )

    design <- design |>
      dplyr::mutate(type = "rr_joint")

    object <- rifttable(
      design = design,
      data = breastcancer
    )
    expected <- tibble::tribble(
      ~Stage, ~`Stage I`,         ~`Stage II`,        ~`Stage III`,
      "Low",  "1.83 (0.40, 8.4)", "4.5 (1.70, 12)",   "9.4 (4.0, 22)",
      "High", "1 (reference)",    "2.53 (0.99, 6.4)", "6.6 (2.60, 17)"
    )
    expect_equal(
      object = object,
      expected = expected
    )

    expect_error(
      object = design |>
        dplyr::mutate(stratum = "") |>
        rifttable(data = breastcancer),
      regexp = "stratum cannot be an empty string"
    )

    expect_error(
      object = design |>
        dplyr::mutate(stratum = NA) |>
        rifttable(data = breastcancer),
      regexp = "or missing"
    )

    expect_error(
      object = rifttable(
        design = design |>
          dplyr::select(-"stratum"),
        data = breastcancer
      ),
      regexp = "stratum must be specified"
    )

    expect_error(
      object = rifttable(
        design = design |>
          dplyr::mutate(stratum = NULL),
        data = breastcancer
      ),
      regexp = "stratum must be specified"
    )

    expect_error(
      object = rifttable(
        design = design |>
          dplyr::select(-"stratum"),
        data = breastcancer
      ),
      regexp = "stratum must be specified"
    )
  }
)
