# TODO: document and export
aermet_job_options <- function(
  REPORT = NULL,
  DEBUG = NULL,
  CHK_SYNTAX = FALSE,
  NOPRINT = FALSE,
  ...
) {
  stopifnot(
    is.null(REPORT) ||
      (is.character(REPORT) && length(REPORT) == 1),
    is.null(DEBUG) ||
      (is.character(DEBUG) && length(DEBUG) == 1),
    is.logical(CHK_SYNTAX) & length(CHK_SYNTAX) == 1,
    is.logical(NOPRINT) & length(NOPRINT) == 1
  )
  rlang::list2(
    REPORT = REPORT,
    CHK_SYNTAX = CHK_SYNTAX,
    NOPRINT = NOPRINT,
    DEBUG = DEBUG,
    ...
  ) |>
    purrr::compact()
}

aermet_surface_options <- function(
  EXTRACT = NULL,
  XDATES = NULL,
  QAOUT = NULL,
  AUDIT = NULL,
  RANGE = NULL,
  NO_MISSING = NULL,
  ASOS1MIN = NULL,
  ...
) {
  rlang::list2(
    EXTRACT = EXTRACT,
    XDATES = XDATES,
    QAOUT = QAOUT,
    AUDIT = AUDIT,
    RANGE = RANGE,
    NO_MISSING = NO_MISSING,
    ASOS1MIN = ASOS1MIN,
    ...
  ) |>
    purrr::compact()
}

aermap_upperair_options <- function(
  EXTRACT = NULL,
  XDATES = NULL,
  QAOUT = NULL,
  AUDIT = NULL,
  RANGE = NULL,
  NO_MISSING = NULL,
  MODIFY = NULL,
  ...
) {
  rlang::list2(
    EXTRACT = EXTRACT,
    XDATES = XDATES,
    QAOUT = QAOUT,
    AUDIT = AUDIT,
    RANGE = RANGE,
    NO_MISSING = NO_MISSING,
    MODIFY = MODIFY,
    ...
  ) |>
    purrr::compact()
}

aermap_onsite_prog_options <- function(
  XDATES = NULL,
  QAOUT = NULL,
  AUDIT = NULL,
  RANGE = NULL,
  NO_MISSING = NULL,
  OSHEIGHTS = NULL,
  DELTA_TEMP = NULL,
  THRESHOLD = NULL,
  OBS_HOUR = NULL, # subbed for "OBS/HOUR"
  ...
) {
  rlang::list2(
    XDATES = XDATES,
    QAOUT = QAOUT,
    AUDIT = AUDIT,
    RANGE = RANGE,
    NO_MISSING = NO_MISSING,
    OSHEIGHTS = OSHEIGHTS,
    DELTA_TEMP = DELTA_TEMP,
    THRESHOLD = THRESHOLD,
    `OBS/HOUR` = OBS_HOUR,
    ...
  ) |>
    purrr::compact()
}

aermet_metprep_options <- function(
  MODEL = NULL,
  THRESH_1MIN = NULL,
  LOCATION = NULL,
  NWS_HGT = NULL,
  XDATES = NULL,
  METHOD = NULL,
  UAWINDOW = NULL,
  AERSURF = NULL,
  AERSURF2 = NULL,
  SSTDEPTH = NULL,
  ZI_GUST = NULL,
  ZI_MIN = NULL,
  MIN_MOL = NULL,
  DEF_VPTG = NULL,
  ...
) {
  rlang::list2(
    MODEL = MODEL,
    THRESH_1MIN = THRESH_1MIN,
    LOCATION = LOCATION,
    NWS_HGT = NWS_HGT,
    XDATES = XDATES,
    METHOD = METHOD,
    UAWINDOW = UAWINDOW,
    AERSURF = AERSURF,
    AERSURF2 = AERSURF2,
    SSTDEPTH = SSTDEPTH,
    ZI_GUST = ZI_GUST,
    ZI_MIN = ZI_MIN,
    MIN_MOL = MIN_MOL,
    DEF_VPTG = DEF_VPTG,
    ...
  ) |>
    purrr::compact()
}

# FORMATTERS --------------------------------------------------------------

format_aermet_job_options <- function(
  options = aermet_job_options(),
  expand_paths = TRUE,
  n_spaces_start = 4,
  n_spaces_after_keys = NA
) {
  flag_opts <- c("CHK_SYNTAX", "NOPRINT")
  if (any(flag_opts %in% names(options))) {
    options[flag_opts] <- options[flag_opts] |>
      format_flag_options()
    options <- options |> purrr::compact()
  }

  path_opts <- c("MESSAGES", "REPORT", "DEBUG")
  if (any(path_opts %in% names(options))) {
    path_opts <- path_opts[path_opts %in% names(options)]
    options[path_opts] <- options[path_opts] |>
      lapply(format_path_options, expand_paths = expand_paths)
  }

  if (is.na(n_spaces_after_keys)) {
    key_lengths <- nchar(names(options))
    n_spaces_after_keys <- (max(key_lengths) - key_lengths) + 2
  }
  options |>
    purrr::compact() |>
    format_options(
      n_spaces_start = n_spaces_start,
      n_spaces_after_keys = n_spaces_after_keys
    )
}

format_aermet_surface_options <- function(
  options = aermet_surface_options(),
  expand_paths = TRUE,
  n_spaces_start = 4,
  n_spaces_after_keys = NA
) {
  path_opts <- c("EXTRACT", "QAOUT", "ASOS1MIN")
  if (any(path_opts %in% names(options))) {
    options[path_opts] <- options[path_opts] |>
      lapply(format_path_options, expand_paths = expand_paths)
  }

  list_opts <- c("AUDIT", "NO_MISSING")
  if (any(list_opts %in% names(options))) {
    options[list_opts] <- options[list_opts] |>
      lapply(format_list_option)
  }

  options |>
    format_options(
      n_spaces_start = n_spaces_start,
      n_spaces_after_keys = n_spaces_after_keys
    )
}

format_aermet_upperair_options <- function(
  options = aermap_upperair_options(),
  expand_paths = TRUE,
  n_spaces_start = 4,
  n_spaces_after_keys = NA
) {
  path_opts <- c("EXTRACT", "QAOUT")
  if (any(path_opts %in% names(options))) {
    options[path_opts] <- options[path_opts] |>
      lapply(format_path_options, expand_paths = expand_paths)
  }

  list_opts <- c("AUDIT", "NO_MISSING")
  if (any(list_opts %in% names(options))) {
    options[list_opts] <- options[list_opts] |>
      lapply(format_list_option)
  }

  options |>
    format_options(
      n_spaces_start = n_spaces_start,
      n_spaces_after_keys = n_spaces_after_keys
    )
}

format_aermap_onsite_and_prog_options <- function(
  options = aermap_onsite_prog_options(),
  n_spaces_start = 4,
  n_spaces_after_keys = NA
) {
  list_opts <- c("AUDIT", "NO_MISSING", "OSHEIGHTS")
  if (any(list_opts %in% names(options))) {
    options[list_opts] <- options[list_opts] |>
      lapply(format_list_option)
  }

  if ("OBS/HOUR" %in% names(options)) {
    options$`OBS/HOUR` <- "%s %s" |>
      sprintf(options$`OBS/HOUR`[[1]], options$`OBS/HOUR`[[2]])
  }
  if ("DELTA_TEMP" %in% names(options)) {
    options$DELTA_TEMP <- "%s %s %s" |>
      sprintf(
        options$DELTA_TEMP[[1]],
        options$DELTA_TEMP[[2]],
        options$DELTA_TEMP[[3]]
      )
  }

  options |>
    format_options(
      n_spaces_start = n_spaces_start,
      n_spaces_after_keys = n_spaces_after_keys
    )
}

format_aermet_metprep_options <- function(
  options = aermet_metprep_options(),
  expand_paths = TRUE,
  n_spaces_start = 4,
  n_spaces_after_keys = NA
) {
  path_opts <- c("EXTRACT", "QAOUT")
  if (any(path_opts %in% names(options))) {
    options[path_opts] <- options[path_opts] |>
      lapply(format_path_options, expand_paths = expand_paths)
  }

  list_opts <- c("AUDIT", "NO_MISSING")
  if (any(list_opts %in% names(options))) {
    options[list_opts] <- options[list_opts] |>
      lapply(format_list_option)
  }

  if ("METHOD" %in% names(options)) {
    options$METHOD <- "%s %s" |>
      sprintf(options$METHOD[[1]], options$METHOD[[2]])
  }

  if ("LOCATION" %in% names(options)) {
    options$LOCATION = options$LOCATION |> format_location_option()
  }

  options |>
    format_options(
      n_spaces_start = n_spaces_start,
      n_spaces_after_keys = n_spaces_after_keys
    )
}
