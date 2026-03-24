#' AERMET Job Pathway Optional Settings
#'
#' @param REPORT Summary report file path (default: NULL)
#'   If NULL, then the summary is written to **"the output control device connected to logical unit 6. On a personal computer, this unit is normally the video monitor."**
#' @param CHK_SYNTAX Whether to check the syntax of the input file prior to processing (default: FALSE)
#' @param NOPRINT Whether to suppress the output of the model to the active session (default: FALSE)
#' @param DEBUG Debug file path (default: NULL)
#'   If NULL, no debug file is created.
#'   Note, the debug file can get large depending on the model inputs.
#' @param ... Additional named args (for future-proofing)
#' @export
aermet_job_options <- function(
  REPORT = NULL,
  CHK_SYNTAX = FALSE,
  NOPRINT = FALSE,
  DEBUG = NULL,
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

#' AERMET Surface Pathway Optional Settings
#'
#' @param EXTRACT Where to write data retrieved from the archive file (default: NULL)
#'   If NULL, no data is extracted from the archive file.
#' @param XDATES Date range (Date objects) to extract from the archive file (default: NULL)
#'   If NULL, all data are extracted from the archive file if `EXTRACT` is not NULL.
#' @param QAOUT Quality assurance output file path (default: NULL).
#'   If NULL, no QA is performed and no output is written.
#'   If not NULL, dry bulb temperature, wind speed, and wind direction (as well as any variables specified in `AUDIT`) are QA'd and summarized to the QAOUT file.
#' @param AUDIT Additional variables to QA or "ALL" for all variables (default: NULL).
#'   If NULL, no additional QA is performed.
#' @param RANGE Upper/lower limits and missing value indicators of variables for QA (default: NULL).
#'   If NULL, defaults defined in Table B-1 of the AERMET User's Guide are used (https://gaftp.epa.gov/Air/aqmg/SCRAM/models/met/aermet/aermet_userguide.pdf).
#' @param NO_MISSING Whether to suppress missing value indicators in QA output for specified variables (default: NULL).
#' @param ASOS1MIN Path to the ASOS1MIN file if using wind speed and wind direction derived from 1-minute or 5-minute ASOS wind observations available from the NCEI (default: NULL).
#' @param ... Additional named args (for future-proofing)
#' @export
aermet_surface_options <- function(
  EXTRACT = list(NULL, "path/to/file")[[1]],
  XDATES = list(NULL, as.Date(c("2000-01-01", "2000-01-02")))[[1]],
  QAOUT = list(NULL, "path/to/file")[[1]],
  AUDIT = list(NULL, c("var1", "var2"), "ALL")[[1]],
  RANGE = list(
    NULL,
    data.frame(
      name = "var1",
      min = 1,
      symbol = "<=",
      max = 2,
      missing_indicator = "-9"
    )
  )[[1]],
  NO_MISSING = list(NULL, c("var1", "var2"), "ALL")[[1]],
  ASOS1MIN = list(NULL, "path/to/file")[[1]],
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

#' AERMET Upperair Pathway Optional Settings
#'
#' @inheritParams aermet_surface_options
#' @param QAOUT Quality assurance output file path (default: NULL).
#'   If NULL, no QA is performed and no output is written.
#'   If not NULL, variables specified in `AUDIT` are QA'd and summarized to the QAOUT file.
#' @param MODIFY Whether to enable various modifications for upper-air data on extraction (default: NULL).
#'   If NULL, no modifications are performed.
#'   If not NULL, can be one or more of the following, or "ALL":
#'   "DELMAND" (delete mandatory levels from sounding),
#'   "CALMDIR" (set non-zero wind dir to 0 if wind speed is 0), or
#'   "SUB_TTTD" (missing ambient and dew point temperatures are replaced by interpolated values)
aermet_upperair_options <- function(
  EXTRACT = list(NULL, "path/to/file")[[1]],
  XDATES = list(NULL, as.Date(c("2000-01-01", "2000-01-02")))[[1]],
  QAOUT = list(NULL, "path/to/file")[[1]],
  AUDIT = list(NULL, c("var1", "var2"), "ALL")[[1]],
  RANGE = list(
    NULL,
    data.frame(
      name = "var1",
      min = 1,
      symbol = "<=",
      max = 2,
      missing_indicator = "-9"
    )
  )[[1]],
  NO_MISSING = list(NULL, c("var1", "var2"), "ALL")[[1]],
  MODIFY = list(NULL, "DELMAND", "CALMDIR", "SUB_TTTD")[[1]],
  ...
) {
  stopifnot(
    "AUDIT is required if QAOUT is specified for UPPERAIR" = is.null(AUDIT) |
      (!is.null(QAOUT) & !is.null(AUDIT)),
  )
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

#' AERMET Onsite or Prognostic Pathway Optional Settings
#'
#' @inheritParams aermet_surface_options
#' @param OSHEIGHTS an altnerate way to provide measurement heights for multi-level profile data (default: NULL).
#'   If not NULL, must be a vector of measurement heights (in meters) for each level of the profile.
#' @param DELTA_TEMP Defines the lower and upper measurement heights (m) for onsite temperature difference variable(s) (DT01–DT03),
#'   which represent directly measured temperature gradients rather than differences between independent temperature sensors.
#'   Only DT01 (index = 1) is used in later processing stages of AERMET (default: NULL).
#'   If not NULL, must be a data.frame with columns "lower" and "upper" of measurement heights (in meters) for each level of the profile (to a maximum of 3 levels).
#' @param OBS_HOUR "OBS/HOUR" parameter - defines how many observations per hour for sub-hourly data (default: NULL).
aermet_onsite_prog_options <- function(
  XDATES = list(NULL, as.Date(c("2000-01-01", "2000-01-02")))[[1]],
  QAOUT = list(NULL, "path/to/file")[[1]],
  AUDIT = list(NULL, c("var1", "var2"), "ALL")[[1]],
  RANGE = list(
    NULL,
    data.frame(
      name = "var1",
      min = 1,
      symbol = "<=",
      max = 2,
      missing_indicator = "-9"
    )
  )[[1]],
  NO_MISSING = list(NULL, c("var1", "var2"), "ALL")[[1]],
  OSHEIGHTS = list(NULL, c(1, 2, 3))[[1]],
  DELTA_TEMP = list(
    NULL,
    data.frame(lower = c(0, 10, 100), upper = c(10, 100, 1000))
  )[[1]],
  OBS_HOUR = list(NULL, 6), # subbed for "OBS/HOUR"
  ...
) {
  rlang::list2(
    XDATES = XDATES,
    QAOUT = list(NULL, "path/to/file")[[1]],
    AUDIT = list(NULL, c("var1", "var2"), "ALL")[[1]],
    RANGE = RANGE,
    NO_MISSING = list(NULL, c("var1", "var2"), "ALL")[[1]],
    OSHEIGHTS = sort(OSHEIGHTS),
    DELTA_TEMP = DELTA_TEMP,
    `OBS/HOUR` = OBS_HOUR,
    ...
  ) |>
    purrr::compact()
}

#' AERMET MetPrep Pathway Optional Settings
#'
#' @inheritParams aermet_surface_options
#' @param MODEL Model name (default: NULL - assumes "AERMOD").
#' @param THRESH_1MIN Threshold  wind speed for the 1-minute ASOS data (default: NULL - assumes 0.5 m/s).
#' @param LOCATION Location used for time conversion -
#'  only needed when site-specific mixing heights are provided during Stage 1 and
#'  upper air sounding data are omitted from the processing (default: NULL).
#'  If not NULL, must be a list with elements "site_id", "lat", "lng", and optionally "tz_offset".
#' @param METHOD define processing methods for the input data including data substitution,
#'  special treatment of ASOS wind data, stable boundary layer treatment, and upper air sounding selection.
#'  (default: NULL).
#'  If not NULL, must be a list with one or more entries named
#'  "WIND_DIR", "REFLEVEL", "ASOS_ADJ", "UASELECT", "STABLEBL", "CCVR", "TEMP", and/or "COARE",
#'  each of which must be a vector with one or more named logical parameters (see details).
#' @param UAWINDOW Window for selecting upper air sounding data as a list with elements "start" and "end" 
#'  with the hour difference relative to the reference sounding times (default: NULL - equivelant to `list(start = -1, end = 1)`). 
#' @param AERSURF,AERSURF2 Primary/secondary surface characteristic file paths (default: NULL).
#' @param SSTDEPTH Sea surface temperature and measurement depth (default: NULL).
#'  If not null, must be a list with elements "SST" and "DEPTH" specifying 
#'  the sea surface temperature (in degrees C) and depth of measurement (in metres).
#' @param ZI_GUST,ZI_MIN  override the default mixing height to use for gust calculations and 
#'  the default minimum mixing height used in COARE. Units are metres. (defaults: NULL - equivelant to 600 and 25)
#' @param MIN_MOL minimum absolute value of Monin-Obukov length allowed. Units are metres. (default: NULL - equivelant to 5).
#' @param DEF_VPTG default value of potential temperature gradient output by COARE. Units are degrees C per metre. (default: NULL - equivelant to 0.01).
#' 
#' @details
#'   METHOD parameters: # TODO:
aermet_metprep_options <- function(
  MODEL = list(NULL, "AERMOD")[[1]],
  THRESH_1MIN = list(NULL, 0.5)[[1]],
  LOCATION = list(
    NULL,
    list(
      site_id = "123456",
      lat = 50.123,
      lng = -175.123,
      tz_offset = 7
    )
  )[[1]],
  XDATES = list(NULL, as.Date(c("2000-01-01", "2000-01-02")))[[1]],
  METHOD = NULL,
  UAWINDOW = list(NULL, list(start = -1, end = 1))[[1]],
  AERSURF = list(NULL, "path/to/file")[[1]], # TODO: include ability to pass years list
  AERSURF2 = list(NULL, "path/to/file")[[1]],
  SSTDEPTH = list(NULL, list(SST = 0, DEPTH = 0))[[1]],
  ZI_GUST = list(NULL, 600)[[1]],
  ZI_MIN = list(NULL, 25)[[1]],
  MIN_MOL = list(NULL, 5)[[1]],
  DEF_VPTG = list(NULL, 0.01)[[1]],
  ...
) {
  rlang::list2(
    MODEL = MODEL,
    THRESH_1MIN = THRESH_1MIN,
    LOCATION = LOCATION,
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
  options = aermet_upperair_options(),
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

format_aermet_onsite_and_prog_options <- function(
  options = aermet_onsite_prog_options(),
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
    names(options$METHOD) |>
      lapply(\(process) {
        if (process == "WIND_DIR") {
          options$METHOD[[process]] |>
            ifelse("RANDOM", "NORAND")
        } else if (process == "REFLEVEL") {
          if (options$METHOD[[process]]) "SUBNWS" else NULL
        } else if (process == "ASOS_ADJ") {
          if (options$METHOD[[process]]) "NO_ADJ" else NULL
        } else if (process == "STABLEBL") {
          c(
            if (options$METHOD[[process]]$BULKRN) "BULKRN" else NULL,
            if (options$METHOD[[process]]$`ADJ_U*`) "ADJ_U*" else NULL
          )
        } else if (process == "CCVR") {
          if (options$METHOD[[process]]) "SUB_CC" else "NO_SUB"
        } else if (process == "TEMP") {
          if (options$METHOD[[process]]) "SUB_TT" else "NOTSUB"
        } else if (process == "COARE") {
          c(
            if (options$METHOD[[process]]$RUN) "RUN_COARE" else "NO_COARE",
            if (options$METHOD[[process]]$WARM) "WARM" else "NO_WARM",
            if (options$METHOD[[process]]$COOL) "COOL" else "NO_COOL",
            if (options$METHOD[[process]]$`ZO_U*`) {
              "ZO_U*"
            } else if (options$METHOD[[process]]$`ZO_U*_PER`) {
              "ZO_U*_PER"
            } else if (options$METHOD[[process]]$ZO_PER_HT) {
              "ZO_PER_HT"
            } else {
              NULL
            },
            if (options$METHOD[[process]]$MIX_DFAULT) {
              "MIX_DFAULT"
            } else if (options$METHOD[[process]]$MIX_OBS) {
              "MIX_OBS"
            } else if (options$METHOD[[process]]$ZIC_OBS_NS) {
              "ZIC_OBS_NS"
            } else if (options$METHOD[[process]]$ZIC_OBS_NS) {
              "CALC_ZI_NS"
            } else if (options$METHOD[[process]]$ZIC_OBS_NS) {
              "ZIC_OBS_SM"
            } else if (options$METHOD[[process]]$CALC_ZI_SM) {
              "CALC_ZI_SM"
            } else {
              NULL
            }
          )
        } else if (process == "UASELECT") {
          if (options$METHOD[[process]]$UASELECT) "SUNRISE" else NULL
        } else {
          NULL
        }
      })

    options$METHOD <- "%s %s" |>
      sprintf(options$METHOD[[1]], options$METHOD[[2]])
  }

  if ("LOCATION" %in% names(options)) {
    options$LOCATION = options$LOCATION |> format_location_option()
  }

  if ("SSTDEPTH" %in% names(options)) {
    options$SSTDEPTH <- "%s %s" |>
      sprintf(options$SSTDEPTH[[1]], options$SSTDEPTH[[2]])
  }

  options |>
    format_options(
      n_spaces_start = n_spaces_start,
      n_spaces_after_keys = n_spaces_after_keys
    )
}
