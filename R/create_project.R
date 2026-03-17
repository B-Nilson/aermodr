create_project <- function(
  path,
  models = c("aermod", "aermap", "aermet"),
  remove_zips = TRUE,
  verbose = TRUE,
  open = rlang::is_interactive(),
  ...
) {
  # setup project
  ignores <- c("*.exe", "*.zip", "*.los", "*.las", "aermap_readme.txt")
  usethis::create_project(path, open = FALSE, ...)
  usethis::with_project(
    path,
    {
      renv::activate()
      renv::snapshot(prompt = FALSE)
      usethis::use_air()
      usethis::use_git_ignore(ignores = ignores)
      usethis::use_git(message = "Initial commit")
    }
  )

  # install models
  for (model in models) {
    install_model(
      path = path,
      model = model,
      verbose = verbose,
      remove_zips = remove_zips
    )
  }

  # Open project if desired
  if (open) {
    usethis::proj_activate(path)
  }
  invisible(path)
}
