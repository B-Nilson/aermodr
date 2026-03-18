#' Download and unzip a file
#'
#' @param zip_url URL of the zip file to download
#' @param local_dir Local directory to download and unzip the file to
#' @param remove_zips Whether to remove the downloaded zip file afterwards (default: TRUE)
#' @param verbose Whether to print a success message after downloading and unzipping (default: TRUE)
#' @param prompt Whether to prompt the user before downloading/unzipping (default: TRUE if R is interactive)
#' @return The path to the unzipped directory
get_and_unzip <- function(
  zip_url,
  local_dir,
  remove_zips = TRUE,
  verbose = TRUE,
  prompt = rlang::is_interactive()
) {
  local_dir |> dir.create(recursive = TRUE, showWarnings = FALSE)
  local_zip <- file.path(local_dir, basename(zip_url))
  if (prompt) {
    okay_to_download <- menu(
      title = "Okay to download and unzip" |>
        paste(zip_url, "?"),
      choices = c("Yes", "No", "Absolutely not")
    )
  } else {
    okay_to_download <- 1
  }
  if (okay_to_download != 1) {
    cli::cli_abort("User aborted download of {zip_url}.")
  }

  zip_url |> download.file(destfile = local_zip, mode = "wb", quiet = !verbose)
  if (remove_zips) {
    on.exit(unlink(local_zip), add = TRUE)
  }
  local_zip |>
    unzip(exdir = local_dir) |>
    invisible()
}
