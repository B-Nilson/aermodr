#' Create a new AERMOD R project
#'
#' @inheritParams get_and_unzip
#' @inheritParams check_installed
#' @param path The path to the project directory to be created.
#' @param models A character vector of models to include in the project. The
#'   models must be one of "aermod", "aermap", or "aermet".
#' @param open Whether to open the project directory in the file explorer after
#'   creation.
#' @return The path to the project directory.
#' @export
create_project <- function(
  path,
  models = c("aermod", "aermap", "aermet"),
  remove_zips = TRUE,
  verbose = TRUE,
  prompt = rlang::is_interactive(),
  open = rlang::is_interactive()
) {
  # setup project
  ignores <- c(
    "*.exe",
    "*.las",
    "*.log",
    "*.los",
    "*.msg",
    "*.OUT",
    "*.out",
    "*.oqa",
    "*.pfl",
    "*.rec",
    "*.rpt",
    "*.src",
    "*.sum",
    "*.sfc",
    "*.zip"
  )
  usethis::create_project(path, open = FALSE)
  usethis::with_project(
    path,
    {
      renv::activate()
      usethis::use_air()
      usethis::use_git_ignore(ignores = ignores)
      usethis::use_git()
    }
  )

  # TODO: add script templates to R/ for running models

  # install models
  for (model in models) {
    install_model(
      path = path,
      model = model,
      verbose = verbose,
      prompt = prompt,
      remove_zips = remove_zips
    )
  }

  # Open project if desired
  if (open) {
    usethis::proj_activate(path)
  }
  invisible(path)
}
