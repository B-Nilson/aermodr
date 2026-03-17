#' Check if AERMOD is installed
#'
#' @param path Path to AERMOD executable (optional). If provided and does not end in `exe_name`, it will be appended.
#' @param exe_name Name of the AERMOD executable (default: "aermod.exe" on Windows, "aermod" otherwise)
#' @param verbose Whether to print a success message if AERMOD is found (default: TRUE)
#' @return A list containing the found path, the source of the path (either "user", "PATH", or "common_path"), and whether the path was found.
#' @export
check_installed <- function(
  path = NULL,
  exe_name = ifelse(.Platform$OS.type == "windows", "aermod.exe", "aermod"),
  verbose = TRUE
) {
  if (!is.null(path)) {
    path <- ifelse(
      path |> endsWith(exe_name),
      path,
      file.path(path, exe_name)
    )
    install_test <- path |> check_user_path(verbose = verbose)
    if (!is.null(install_test)) {
      return(install_test)
    }
  }

  install_test <- exe_name |> check_PATH(verbose = verbose)
  if (!is.null(install_test)) {
    return(install_test)
  }

  install_test <- exe_name |> check_common_locations(verbose = verbose)
  if (!is.null(install_test)) {
    return(install_test)
  }

  PATH <- Sys.getenv('PATH')
  cli::cli_warn(c(
    "AERMOD executable not found.",
    "• Checked for {exe_name} in:",
    "- PATH ({PATH})",
    "- common install locations",
    if (!is.null(path)) "- supplied path {.path path}",
    "• To fix:",
    "1. Install AERMOD from the U.S. EPA:",
    "https://www.epa.gov/scram/air-quality-dispersion-modeling-preferred-and-recommended-models#aermod",
    "2. Add the folder containing '{exe_name}' to your system PATH if not already included",
    "or supply it explicitly:",
    "`check_installed(path = \"C:/path/to/aermod.exe\")`"
  ))
}

check_user_path <- function(path, verbose = TRUE) {
  is_exec <- \(p) {
    !is.null(p) && nzchar(p) && file.exists(p) && file.access(p, mode = 1)
  }
  path <- path |>
    normalizePath(winslash = "/", mustWork = FALSE)

  if (is_exec(path)) {
    if (verbose) {
      cli::cli_alert_success("AERMOD found at {.path {path}}")
    }
    list(
      found = TRUE,
      path = path,
      source = "user"
    )
  } else {
    cli::cli_warn("Supplied AERMOD path does not exist: {.path {path}}")
    invisible()
  }
}

check_PATH <- function(exe_name, verbose = TRUE) {
  path_found <- Sys.which(exe_name)

  if (nzchar(path_found)) {
    if (verbose) {
      cli::cli_alert_success("AERMOD found at {.path {path}}")
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

check_common_locations <- function(exe_name, verbose = TRUE) {
  candidates <- c(
    # Windows
    "C:/AERMOD/aermod.exe",
    "C:/Program Files/AERMOD/aermod.exe",
    "C:/Program Files (x86)/AERMOD/aermod.exe",
    # Linux etc
    "/usr/local/bin/aermod",
    "/usr/bin/aermod",
    "/opt/aermod/bin/aermod",
    "/usr/lib/aermod/bin/aermod",
    "~/aermod/aermod",
    "/usr/local/aermod/bin/aermod",
    "/usr/local/aermod-*/bin/aermod"
  )

  candidates <- candidates |>
    normalizePath(winslash = "/", mustWork = FALSE)

  existing <- candidates[file.exists(candidates)]

  if (length(existing) > 0) {
    path <- existing[1]
    if (verbose) {
      cli::cli_alert_success("AERMOD found at {.path {path}}")
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
