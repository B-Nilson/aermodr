#' Download and unzip a file
#'
#' @param zip_url URL of the zip file to download
#' @param local_dir Local directory to download and unzip the file to
#' @param remove_zips Whether to remove the downloaded zip file afterwards (default: TRUE)
#' @param timeout Timeout for downloading the zip file in seconds (default: 3 minutes)
#' @param verbose Whether to print a success message after downloading and unzipping (default: TRUE)
#' @param prompt Whether to prompt the user before downloading/unzipping (default: TRUE if R is interactive)
#' @return The path to the unzipped directory
get_and_unzip <- function(
  zip_url,
  local_dir,
  remove_zips = TRUE,
  timeout = 60 * 3,
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

  current_timeout <- options("timeout")
  on.exit(options(timeout = current_timeout), add = TRUE)
  options(timeout = timeout)
  zip_url |>
    utils::download.file(destfile = local_zip, mode = "wb", quiet = !verbose)

  if (remove_zips) {
    on.exit(unlink(local_zip), add = TRUE)
  }
  local_zip |>
    utils::unzip(exdir = local_dir) |>
    invisible()
}

format_path_options <- function(
  path,
  expand_paths = TRUE,
  collapse = TRUE
) {
  if (is.null(path)) {
    return(NULL)
  }
  if (expand_paths) {
    path <- path |>
      normalizePath(winslash = "/", mustWork = FALSE)
  } else {
    path <- path |> gsub(pattern = "\\", replacement = "/", fixed = TRUE)
  }

  # Wrap in quotes if path contains spaces
  path <- ifelse(
    grepl(" ", path),
    paste0('"', path, '"'),
    path
  )

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

# TODO: Handle character limits - split into multiple lines as needed
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

format_xdates_option <- function(XDATES) {
  date_format <- "%Y/%m/%d"
  paste(
    format(XDATES[[1]], date_format),
    "TO",
    format(XDATES[[2]], date_format)
  )
}

format_data_option <- function(
  path,
  format = "",
  is_asos = FALSE,
  land_water = ""
) {
  if (land_water != "") {
    return(paste(path, land_water))
  }
  asos <- if (is_asos && format != "") "ASOS" else NULL
  paste(path, format, asos)
}

# TODO: fix formatting of lat/lng to be like "38.4N 81.9W"
format_location_option <- function(location) {
  if (is.null(location)) {
    return(NULL)
  }
  with(
    location,
    paste(
      site_id,
      lat,
      lng,
      tz_offset,
      if (!is.null(location$elev)) ifelse(!is.na(tz_offset), elev, "")
    )
  )
}

format_range_option <- function(RANGE) {
  with(RANGE, paste(name, min, symbol, max, missing_indicator))
}

format_options <- function(
  options,
  n_spaces_start = 4,
  n_spaces_after_keys = NA
) {
  if (length(options) == 0) {
    return(NULL)
  }

  if ("XDATES" %in% names(options)) {
    options$XDATES <- options$XDATES |> format_xdates_option()
  }
  if ("RANGE" %in% names(options)) {
    options$RANGE <- options$RANGE |> format_range_option()
  }

  if (length(n_spaces_after_keys) == 1) {
    n_spaces_after_keys <- rep(n_spaces_after_keys, length(options))
  }
  key_lengths <- nchar(names(options))
  n_spaces_after_keys <- dplyr::case_when(
    names(options) |> startsWith("**") ~ 1,
    is.na(n_spaces_after_keys) ~ (max(key_lengths) - key_lengths) + 2,
    .default = n_spaces_after_keys
  )

  start_spaces <- names(options) |>
    lapply(\(opt_name) {
      ifelse(
        opt_name |> startsWith("**"),
        "",
        paste(rep(" ", n_spaces_start), collapse = "")
      )
    }) |>
    stats::setNames(names(options))

  after_key_spaces <- n_spaces_after_keys |>
    lapply(\(x) paste(rep(" ", x), collapse = "")) |>
    stats::setNames(names(options))
  names(options) |>
    lapply(\(key) {
      paste0(
        start_spaces[[key]],
        key,
        after_key_spaces[[key]],
        options[[key]]
      )
    }) |>
    unlist()
}
