#' Check if AERMOD, AERMAP, or AERMET is installed
#'
#' @param path Path to the model executable (optional). If provided and does not end in `exe_name`, it will be appended.
#' @param model Name of the model (default: "aermod", must be one of "aermod", "aermap", or "aermet")
#' @param exe_name Name of the model executable (default: "aermod.exe" on Windows, "aermod" otherwise)
#' @param verbose Whether to print a success message if AERMOD is found (default: TRUE)
#' @return A list containing the found path, the source of the path (either "user", "PATH", or "common_path"), and whether the path was found.
#' @export
check_installed <- function(
  path = NULL,
  model = c("aermod", "aermap", "aermet")[1],
  exe_name = model |>
    paste0(ifelse(.Platform$OS.type == "windows", ".exe", "")),
  verbose = TRUE
) {
  stopifnot(
    "`path` must be NULL or a character vector of length 1" = path |>
      is.character() &&
      length(path) == 1 ||
      is.null(path),
    "`model` must be one of 'aermod', 'aermap', or 'aermet'" = model %in%
      c("aermod", "aermap", "aermet") &
      length(model) == 1,
    "`exe_name` must be a character vector of length 1" = exe_name |>
      is.character() &&
      length(exe_name) == 1,
    "`verbose` must be a logical vector of length 1" = verbose |>
      is.logical() &&
      length(verbose) == 1
  )

  if (!is.null(path)) {
    path <- ifelse(
      path |> endsWith(exe_name),
      path,
      file.path(path, exe_name)
    )
    install_test <- path |>
      check_user_path(model = model, verbose = verbose)
    return(install_test)
  }

  install_test <- exe_name |>
    check_PATH(model = model, verbose = verbose)
  if (!is.null(install_test)) {
    return(install_test)
  }

  install_test <- exe_name |>
    check_common_locations(model = model, verbose = verbose)
  if (!is.null(install_test)) {
    return(install_test)
  }

  PATH <- Sys.getenv('PATH')
  cli::cli_warn(c(
    "{toupper(model)} executable not found.",
    "",
    "Checked for {exe_name} in:",
    "- PATH ({PATH})",
    "- common install locations",
    if (!is.null(path)) "- supplied path {.path {path}}",
    "",
    "To fix:",
    "1. Install {toupper(model)} from the U.S. EPA:",
    "https://www.epa.gov/scram/air-quality-dispersion-modeling-preferred-and-recommended-models#{model}",
    "2. Add the folder containing '{exe_name}' to your system PATH if not already included",
    "or supply it explicitly:",
    "`check_installed(path = \"C:/path/to/{exe_name}\")`"
  ))
  invisible()
}

check_user_path <- function(path, model, verbose = TRUE) {
  is_exec <- \(p) {
    !is.null(p) && nzchar(p) && file.exists(p) && file.access(p, mode = 1)
  }
  path <- path |>
    normalizePath(winslash = "/", mustWork = FALSE)
  model <- toupper(model)

  if (is_exec(path)) {
    if (verbose) {
      cli::cli_alert_success("{model} found at {.path {path}}")
    }
    list(
      found = TRUE,
      path = path,
      source = "user"
    )
  } else {
    cli::cli_abort("Supplied {model} path does not exist: {.path {path}}")
  }
}

check_PATH <- function(exe_name, model, verbose = TRUE) {
  path_found <- Sys.which(exe_name)

  if (nzchar(path_found)) {
    if (verbose) {
      model <- toupper(model)
      cli::cli_alert_success("{model} found at {.path {path}}")
    }
    list(
      found = TRUE,
      path = path_found,
      source = "PATH"
    )
  } else {
    invisible()
  }
}

check_common_locations <- function(exe_name, model, verbose = TRUE) {
  candidates <- c(
    # Windows
    "C:/AERMOD/",
    "C:/Program Files/AERMOD/",
    "C:/Program Files (x86)/AERMOD/",
    # Linux etc
    "/usr/local/bin/",
    "/usr/bin/",
    "/opt/aermod/bin/",
    "/usr/lib/aermod/bin/",
    "~/aermod/",
    "/usr/local/aermod/bin/",
    "/usr/local/aermod-*/bin/"
  ) |>
    paste0(exe_name)

  candidates <- candidates |>
    normalizePath(winslash = "/", mustWork = FALSE)

  existing <- candidates[file.exists(candidates)]

  if (length(existing) > 0) {
    path <- existing[1]
    if (verbose) {
      model <- toupper(model)
      cli::cli_alert_success("{model} found at {.path {path}}")
    }
    list(
      found = TRUE,
      path = path,
      source = "common_path"
    )
  } else {
    invisible()
  }
}
