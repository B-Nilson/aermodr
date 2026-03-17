test_that("warning when not installed, no warnings when installed", {
  not_installed <- check_installed("C:/", verbose = FALSE) |>
    expect_warning(
      regexp = "(AERMOD executable not found)|(Supplied AERMOD path does not exist)"
    )

  installed <- check_installed() |>
    expect_message(regexp = "AERMOD found at")
})

test_that("installed object formatted correctly", {
  installed <- check_installed(verbose = FALSE) |>
    expect_named(c("found", "path", "source")) |>
    expect_type("list")

  expect_true(installed$found)
  expect_type(installed$path, "character")
  expect_type(installed$source, "character")
})
