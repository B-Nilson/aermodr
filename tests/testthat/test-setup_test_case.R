test_that("able to setup test case", {
  path <- tempdir() |> file.path("aermod_project")
  on.exit(unlink(path, recursive = TRUE))

  result <- setup_test_case(path, open = FALSE, prompt = FALSE)

  result |>
    sub(
      pattern = normalizePath(path, winslash = "/", mustWork = FALSE),
      replacement = ""
    ) |>
    expect_snapshot()
})
