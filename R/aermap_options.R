#' AERMAP (CO) Control Options
#'
#' @param TITLETWO TITLETWO control option (default: NULL - AERMAP assumes no subtitle).
#'   If not NULL, must be a character string of the subtitle for the model run.
#' @param CHECKS CHECK control option (default: NULL - AERMAP does not check the file).
#'   Only used if terrain_data_type is "DEM".
#'   If not NULL, must be one of TRUE or FALSE - TRUE indicates that the preprocessor should check the input data files for errors.
#' @param TIFFDEBUGS TIFFDEBUG control option (default: NULL - AERMAP does not check the file).
#'   Only used if terrain_data_type is "NED".
#'   If not NULL, must be one of TRUE or FALSE - TRUE indicates that the preprocessor should check the input data files for errors.
#' @param ElevUnits ElevUnit control option (default: NULL - AERMAP does not check the file).
#'   Only used if TIFFDEBUG is TRUE and terrain_data_type is "NED".
#'   If not NULL, must be one of "METERS", "FEET", "DECIFEET", "DECAFEET", "DECIMETERS", "DECAMETERS" - indicates the elevation unit of the input data files.
#' @param TERRHGTS TERRHGTS control option (default: NULL - AERMAP assumes "EXTRACT").
#'   If not NULL, must be one of "EXTRACT" or "PROVIDED".
#'   If "EXTRACT", instructs the preprocessor to determine the terrain heights from the DEM data files provided by the user.
#'   If "PROVIDED", forces the preprocessor to use the user-specified receptor elevations that are entered on the receptor pathway
#'   (also applies to source elevations specified on the optional source pathway)
#' @param FLAGPOLE FLAGPOLE control option (default: NULL - AERMAP assumes 0).
#'   Default numeric value (>= 0) for height of (flagpole) receptors above local ground level.
#' @param DOMAINXY DOMAINXY control option (default: NULL - AERMAP assumes full domain of input data).
#'   If not NULL, must be a numeric vector of UTM coordinates (x, y, and zone) for the lower left and upper right corners of the domain
#'   with named elements xmin, xmax, ymin, ymax, zone_min, and zone_max.
#' @param DOMAINLL DOMAINLL control option (default: NULL - AERMAP assumes full domain of input data).
#'   If not NULL, must be a numeric vector of longitude and latitude coordinates for the lower left and upper right corners of the domain
#'   with named elements xmin, xmax, ymin, and ymax.
#' @param NADGRIDS NADGRIDS control option (default: "NAD_gridshifts/").
#'   If NULL, AEMAP assumes the same directory as the input file.
#'   If not NULL, must be a character string of the path to the folder containing NADCON grid shift files (.las and .los).
#' @param DEBUGOPT DEBUGOPT control option (default: NULL - AERMAP does not print debug information).
#'   If not NULL, must be a character value equal to "ALL" (equivalent to `c("HILL", "RECEPTOR", "SOURCE")`), or a vector of 1 or more of "HILL", "RECEPTOR", or "SOURCE".
#'   Separate debug files are created for each of these options to document the determination of the:
#'   critical hill height scales,
#'   receptor elevations and coordinate conversions,
#'   and source elevations and coordinate conversions
#' @param ... Additional named args (for future-proofing)
#'
#' @return A list of set control options for AERMAP.
#' @references
#' U.S. Environmental Protection Agency (EPA). (2018).
#' *AERMAP User's Guide* (Version 18081), Table B-2.
#' https://gaftp.epa.gov/aqmg/SCRAM/models/related/aermap/aermap_userguide_v18081.pdf
#'
#' @export
aermap_control_options <- function(
  TITLETWO = list(NULL, "Subtitle")[[1]],
  CHECKS = list(NULL, TRUE, FALSE)[[1]],
  TIFFDEBUGS = list(NULL, "path(s)/to/debug/file(s)")[[1]],
  ElevUnits = list(
    NULL,
    "FEET",
    "DECIFEET",
    "DECAFEET",
    "METERS",
    "DECIMETERS",
    "DECAMETERS"
  )[[1]],
  TERRHGTS = list(NULL, "EXTRACT", "PROVIDED")[[1]],
  FLAGPOLE = list(NULL, 0)[[1]],
  DOMAINXY = c(
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,
    zone_min = NULL,
    zone_max = NULL
  ),
  DOMAINLL = c(xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL),
  NADGRIDS = "NAD_gridshifts/",
  DEBUGOPT = list(NULL, "HILL", "RECEPTOR", "SOURCE", "ALL")[[1]],
  ...
) {
  stopifnot(
    is.null(TITLETWO) ||
      (is.character(TITLETWO) && length(TITLETWO) == 1),
    is.null(CHECKS) ||
      (is.logical(CHECKS) && length(CHECKS) > 0),
    is.null(TIFFDEBUGS) ||
      (is.character(TIFFDEBUGS) && length(TIFFDEBUGS) > 0),
    is.null(ElevUnits) ||
      (ElevUnits %in%
        c(
          "FEET",
          "DECIFEET",
          "DECAFEET",
          "METERS",
          "DECIMETERS",
          "DECAMETERS"
        ) &
        length(ElevUnits) > 0),
    is.null(TERRHGTS) ||
      (TERRHGTS %in% c("EXTRACT", "PROVIDED") & length(TERRHGTS) == 1),
    is.null(FLAGPOLE) ||
      (is.numeric(FLAGPOLE) && FLAGPOLE >= 0 && length(FLAGPOLE) == 1),
    is.null(DOMAINXY) ||
      (is.numeric(DOMAINXY) &&
        all(
          names(DOMAINXY) %in%
            c("xmin", "xmax", "ymin", "ymax", "zone_min", "zone_max")
        ) &&
        !anyNA(DOMAINXY)),
    is.null(DOMAINLL) ||
      (is.numeric(DOMAINLL) &&
        all(names(DOMAINLL) %in% c("xmin", "xmax", "ymin", "ymax")) &&
        !anyNA(DOMAINLL)),
    is.null(NADGRIDS) ||
      (is.character(NADGRIDS) && length(NADGRIDS) == 1),
    is.null(DEBUGOPT) ||
      all(DEBUGOPT %in% c("HILL", "RECEPTOR", "SOURCE")) ||
      identical(DEBUGOPT, "ALL")
  )

  rlang::list2(
    TITLETWO = TITLETWO,
    CHECK = CHECKS,
    TIFFDEBUG = TIFFDEBUGS,
    ElevUnits = ElevUnits,
    TERRHGTS = TERRHGTS,
    FLAGPOLE = FLAGPOLE,
    DOMAINXY = DOMAINXY,
    DOMAINLL = DOMAINLL,
    NADGRIDS = NADGRIDS,
    DEBUGOPT = DEBUGOPT,
    ...
  ) |>
    purrr::compact()
}

#' AERMAP output options
#'
#' @param DEBUGHIL DEBUGHIL control option (default: NULL - AERMAP does not print HILL debug information).
#'   If not NULL, must be a character string of the path to the HILL debug file.
#' @param DEBUGREC,DEBUGSRC DEBUGREC/DEBUGSRC control option (default: NULL - AERMAP does not print RECEPTOR/SOURCE debug information).
#'   If not NULL, must be a character vector with the paths to the receptor/source NAD conversion and domain check file, the DEM-NED assignment file, and the receptor/source elevation file, in that order.
#' @param ... Additional named args (for future-proofing)
#'
#' @return A list of set output options for AERMAP.
#' @export
aermap_output_options <- function(
  DEBUGHIL = list(NULL, "path/to/HILL/debug/file")[[1]],
  DEBUGREC = list(
    NULL,
    c(
      "path/to/NADconv&DomainCheck/file",
      "path/to/NEDassignment/file",
      "path/to/RECelev/file"
    )
  )[[1]],
  DEBUGSRC = list(
    NULL,
    c(
      "path/to/NADconv&DomainCheck/file",
      "path/to/NEDassignment/file",
      "path/to/SRCelev/file"
    )
  )[[1]],
  ...
) {
  stopifnot(
    is.null(DEBUGHIL) ||
      (is.character(DEBUGHIL) && length(DEBUGHIL) == 1),
    is.null(DEBUGREC) ||
      (is.character(DEBUGREC) && length(DEBUGREC) == 3),
    is.null(DEBUGSRC) ||
      (is.character(DEBUGSRC) && length(DEBUGSRC) == 3)
  )
  rlang::list2(
    DEBUGHIL = DEBUGHIL,
    DEBUGREC = DEBUGREC,
    DEBUGSRC = DEBUGSRC,
    ...
  ) |>
    purrr::compact()
}

format_aermap_control_options <- function(
  options = aermap_control_options(),
  n_spaces_start = 3,
  n_spaces_after_keys = NA
) {
  domain_opts <- c(xy = "DOMAINXY", ll = "DOMAINLL")
  if (any(domain_opts %in% names(options))) {
    options[domain_opts] <- names(options[domain_opts]) |>
      lapply(\(type) {
        options[[domain_opts[[type]]]] |> format_domain_option(type = type)
      })
  }
  if ("DEBUGOPT" %in% names(options)) {
    options$DEBUGOPT <- options$DEBUGOPT |> format_list_option()
  }
  options |>
    format_options(
      n_spaces_start = n_spaces_start,
      n_spaces_after_keys = n_spaces_after_keys
    )
}

format_aermap_output_options <- function(
  options = aermap_output_options(),
  expand_paths = TRUE,
  n_spaces_start = 3,
  n_spaces_after_keys = NA
) {
  names(options) <- toupper(names(options))
  path_opts <- c("DEBUGHIL", "DEBUGREC", "DEBUGSRC")
  if (any(path_opts %in% names(options))) {
    options[path_opts] <- options[path_opts] |>
      lapply(format_path_options, expand_paths = expand_paths)
  }

  options |>
    format_options(
      n_spaces_start = n_spaces_start,
      n_spaces_after_keys = n_spaces_after_keys
    )
}
