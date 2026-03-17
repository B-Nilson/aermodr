test_that("warning when not installed, no warnings when installed", {
  models <- c("aermod", "aermap", "aermet")
  for (model in models) {
    not_installed <- "C:/" |>
      check_installed(model = !!model, verbose = FALSE) |>
      expect_warning(
        regexp = "(%s executable not found)" |>
          sprintf(toupper(model))
      ) |>
      expect_warning(
        regexp = "(Supplied %s path does not exist)" |>
          sprintf(toupper(model))
      )

    installed <- check_installed() |>
      expect_message(
        regexp = "%s found at" |>
          sprintf(toupper(model))
      )
  }
})

test_that("installed object formatted correctly", {
  models <- c("aermod", "aermap", "aermet")
  for(model in models) {
    installed <- check_installed(model = !!model, verbose = FALSE) |>
      expect_named(c("found", "path", "source")) |>
      expect_type("list")

    expect_true(installed$found)
    expect_type(installed$path, "character")
    expect_type(installed$source, "character")
  }
})
