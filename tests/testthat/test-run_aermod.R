test_that("able to run test case", {
  path <- tempdir() |> file.path("aermod_project")
  on.exit(unlink(path, recursive = TRUE))

  path |> setup_test_case(verbose = FALSE, prompt = FALSE, open = FALSE)

  # Setup inputs for AERMOD
  path |>
    paste0("/AERMAP/") |>
    run_aermap(verbose = FALSE) |>
    expect_silent()
  path |>
    paste0("/AERMET/") |>
    run_aermet(verbose = FALSE) |>
    expect_silent()

  result <- path |>
    paste0("/AERMOD/") |>
    run_aermod(verbose = FALSE) |>
    expect_silent()

  result |> cat() |> expect_snapshot()
})
