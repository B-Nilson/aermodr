test_that("windows installation works", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows")
  install_path <- tempdir()
  on.exit(unlink(install_path, recursive = TRUE))
  for (model in models) {
    installed_path <- install_model(
      path = !!install_path,
      model = !!model,
      verbose = FALSE
    ) |>
      expect_silent() |>
      expect_type("character")
    expect_true(file.exists(!!installed_path))
  }
})
