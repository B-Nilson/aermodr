#' Run AERMAP model
#'
#' @param path Path to directory with the model executable (default: ./AERMAP/).
#' @param exe_name Name of the model executable (default: "aermap.exe" on Windows, "aermap" otherwise)
#' @param run_name Name of the run (default: "aermap") - used for default names of the input and output files
#' @param inp_name Name of the input file (default: "{run_name}.inp")
#' @param out_name Name of the output file (default: "{run_name}.out")
#' @param log_name Name of the log file (default: "{run_name}.log")
#' @param verbose Whether to print a success message if AERMAP is found (default: TRUE)
#' @return A character value of the model log (invisible), or an error if the model log is not found (i.e. the model failed to run)
#' @export
run_aermap <- function(
  path = "AERMAP",
  exe_name = "aermap.exe",
  run_name = "aermap",
  inp_name = run_name |> paste0(".inp"),
  out_name = run_name |> paste0(".out"),
  log_name = run_name |> paste0(".log"),
  verbose = TRUE
) {
  is_installed <- check_installed(
    path = path,
    model = "aermap",
    exe_name = exe_name,
    verbose = verbose
  )
  if (is.null(is_installed)) {
    cli::cli_abort("Ensure aermap is installed in {path}")
  }

  old_wd <- getwd()
  setwd(path)
  on.exit(setwd(old_wd), add = TRUE)

  inp_exists <- file.exists(inp_name)
  if (!inp_exists) {
    cli::cli_abort("{inp_name} does not exist relative to {.path {path}}")
  }

  system2(
    command = exe_name,
    args = c(inp_name, out_name),
    stdout = log_name,
    stderr = log_name
  )

  if (file.exists(log_name)) {
    log <- readLines(log_name) |> paste(collapse = "\n")
    if (verbose) {
      cli::cli_alert_info("Model log: {.path {path}/{log_name}}" |> c(log))
    }
    return(invisible(log))
  } else {
    cli::cli_abort("Model log not found: {.path {path}/{log_name}}")
  }
}
