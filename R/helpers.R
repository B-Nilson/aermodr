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
