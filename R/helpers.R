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
    okay_to_download <- utils::menu(
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

  zip_url |>
    utils::download.file(destfile = local_zip, mode = "wb", quiet = !verbose)
  if (remove_zips) {
    on.exit(unlink(local_zip), add = TRUE)
  }
  local_zip |>
    utils::unzip(exdir = local_dir) |>
    invisible()
}

format_path_options <- function(path, expand_paths = TRUE, quote = TRUE, collapse = TRUE) {
  if (is.null(path)) {
    return(NULL)
  }
  if (expand_paths) {
    path <- path |>
      normalizePath(winslash = "/", mustWork = FALSE)
  } else {
    path <- path |> gsub(pattern = "\\", replacement = "/", fixed = TRUE)
  }
  if (quote) {
    path <- paste0('"', path, '"') # enclose in double quotes in case path contains spaces
  } else {
    path <- path |> gsub(pattern = " ", replacement = "\\ ", fixed = TRUE)
  }

  if (collapse) {
    path <- path |> paste(collapse = " ")
  }
  return(path)
}

format_flag_options <- function(flag_options) {
  flag_options |>
    lapply(\(opt) {
      if (opt) {
        return("")
      } else {
        return(NULL)
      }
    })
}

format_list_option <- function(list_option) {
  list_option |>
    unique() |>
    paste(collapse = " ")
}

format_domain_option <- function(domain, type = "xy") {
  domain <- as.list(domain)
  fmt_xy <- \(x) formatC(x, format = "f", digits = 0)
  fmt_ll <- \(x) formatC(x, format = "f", digits = 5, drop0trailing = FALSE)

  if (type == "xy") {
    "%s %s %s  %s %s %s" |>
      sprintf(
        fmt_xy(domain$xmin),
        fmt_xy(domain$ymin),
        domain$zone_min,
        fmt_xy(domain$xmax),
        fmt_xy(domain$ymax),
        domain$zone_max
      )
  } else if (type == "ll") {
    "%s %s  %s %s" |>
      sprintf(
        fmt_ll(domain$xmin),
        fmt_ll(domain$ymin),
        fmt_ll(domain$xmax),
        fmt_ll(domain$ymax)
      )
  }
}

format_options <- function(
  options,
  n_spaces_start = 4,
  n_spaces_after_keys = NA
) {
  if (is.na(n_spaces_after_keys)) {
    key_lengths <- nchar(names(options))
    n_spaces_after_keys <- (max(key_lengths) - key_lengths) + 2
  }
  names(options) |>
    sapply(\(key) {
      "%s%s%s%s" |> sprintf(n_spaces, key, n_spaces_after_keys, options[[key]])
    }) |>
    unlist()
}
