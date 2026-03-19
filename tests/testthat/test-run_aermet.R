test_that("able to run test case", {
  path <- tempdir() |> file.path("aermod_project")
  on.exit(unlink(path, recursive = TRUE))

  path |> setup_test_case(verbose = FALSE, prompt = FALSE, open = FALSE)

  result <- path |>
    paste0("/AERMET/") |>
    run_aermet(verbose = FALSE) |>
    expect_silent()

  result <- result |>
    lapply(\(x) {
      gsub(
        "PROCESSING DATE/TIME: .*?(PM|AM)",
        "PROCESSING DATE/TIME: {CENSORED}",
        x
      )
    })
  {
    for (res in result) {
      cat(res, "\n")
    }
  } |>
    expect_snapshot()
})
