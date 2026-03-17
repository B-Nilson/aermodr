test_that("basic project creation works", {
  path <- tempdir() |> file.path("aermod_project")
  on.exit(unlink(path, recursive = TRUE))

  models <- c("aermod", "aermap", "aermet")
  create_project(path, models = models, open = FALSE)

  expected <- paste0(models, ".zip") |> 
    c(toupper(models))
  list.files(path)
})
