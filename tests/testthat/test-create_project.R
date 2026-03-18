test_that("basic project creation works", {
  path <- tempdir() |> file.path("aermod_project")
  on.exit(unlink(path, recursive = TRUE))

  models <- c("aermod", "aermap", "aermet")
  create_project(path, models = models, open = FALSE, prompt = FALSE)

  expected <- toupper(models) |>
    file.path(paste0(models, ".exe"))
  list.files(path, recursive = TRUE) |>
    expect_contains(expected)
})
