#' Setup test case
#'
#' This function sets up a test case for the aermodr package
#' by downloading and unzipping the SampleRun.zip file from the
#' US EPA Support Center for Regulatory Atmospheric Modeling (SCRAM) website.
#'
#' @inheritParams create_project
#' @inheritParams get_and_unzip
#'
#' @return The path to the test case directory.
#'
#' @export
setup_test_case <- function(
  path,
  remove_zips = TRUE,
  timeout = 60 * 3,
  verbose = TRUE,
  prompt = rlang::is_interactive(),
  open = rlang::is_interactive()
) {
  path <- path |> normalizePath(winslash = "/", mustWork = FALSE)
  test_case_url <- "https://gaftp.epa.gov/Air/aqmg/SCRAM/models/preferred/aermod/SampleRun.zip"
  path |>
    create_project(
      remove_zips = remove_zips,
      verbose = verbose,
      prompt = prompt,
      open = FALSE
    )
  local_files <- test_case_url |>
    get_and_unzip(
      local_dir = path,
      remove_zips = remove_zips,
      verbose = verbose,
      prompt = prompt,
      timeout = timeout
    )
  local_files_new <- local_files |>
    sub(pattern = file.path(path, "SampleRun"), replacement = path)
  local_files |> file.rename(local_files_new)
  file.path(path, "SampleRun") |> unlink(recursive = TRUE)

  if (open) {
    usethis::proj_activate(path)
  }
  invisible(local_files_new)
}
