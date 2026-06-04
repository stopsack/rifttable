test_that(
  desc = "rt_gt works",
  code = {
    expect_visible(
      call =
        tibble::tibble(
          a = "  a",
          b = 1
        ) |>
          rt_gt()
    )
    knitr::opts_knit$set(rmarkdown.pandoc.to = "gfm")
    expect_visible(
      call =
        tibble::tibble(
          a = "  a",
          b = 1
        ) |>
        rt_gt()
    )
  }
)
