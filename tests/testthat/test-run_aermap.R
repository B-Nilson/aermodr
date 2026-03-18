test_that("able to run test case", {
  path <- tempdir() |> file.path("aermod_project")
  on.exit(unlink(path, recursive = TRUE))

  models <- c("aermod", "aermap", "aermet")
  path |> setup_test_case(verbose = FALSE, prompt = FALSE)

  result <- path |>
    file.path("AERMAP") |>
    run_aermap(verbose = FALSE) |> 
    expect_silent()

  result |> cat() |> expect_snapshot()
})
