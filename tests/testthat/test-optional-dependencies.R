test_that(
  desc = "optional dependencies are noted",
  code = {
    data(breastcancer, package = "risks")
    with_mocked_bindings(
      is_package_installed = function(package) FALSE,
      expect_error(
        object = rifttable(
          design = tibble::tibble(
            exposure = "receptor",
            outcome = "death",
            type = "quantreg"
          ),
          data = breastcancer
        ),
        regexp = "package \"quantreg\" must be installed"
      )
    )

    with_mocked_bindings(
      is_package_installed = function(package) FALSE,
      expect_error(
        object = rifttable(
          design = tibble::tibble(
            exposure = "receptor",
            outcome = "death",
            type = "irrrob"
          ),
          data = breastcancer
        ),
        regexp = "package \"sandwich\" must be installed"
      )
    )

    with_mocked_bindings(
      is_package_installed = function(package) FALSE,
      expect_error(
        object = tibble::tibble(a = 1) %>%
          rt_gt(),
        regexp = "package \"gt\" must be installed"
      )
    )
  }
)

