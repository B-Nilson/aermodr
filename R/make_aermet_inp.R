#' Create an AERMET input file
#' 
#' @param inp_path Path to input file (default: "aermet.inp").
#' @param surface_station,upperair_station,onsite_prog_station 
#'   Station location details - must be a data.frame or list with names 
#'   "site_id", "lat", "lng", and optionally "tz_offset" and/or "elev".
#'   `onsite_prog_station` is only required if using an onsite or prognostic station.
#' @param instrument_heights Heights of surface instruments (in meters) with names matching the instrument variable name (default: NULL). 
#' @param site_characteristics Data.frame of site characteristics with columns 
#'   "frequency", "years", "sector_start", "sector_end", "frequency_id", "sector_id", "albedo", "bowen_ratio", and "surface_roughness".
#' @param output_path,profile_path
#'   Paths to output surface and profile files passed to the OUTPUT and PROFILE options.
#' @param surface_path,upperair_path,onsite_prog_path
#'   Paths to archive files passed to each DATA option.
#' @param surface_data_format,upperair_data_format
#'   Formats of archive files passed to each DATA option.
#'   For surface, must be one of "EXTRACT", "CD144", "SCRAM", "SAMSON", "HUSWO", or "ISHD".
#'   For upperair, must be one of "EXTRACT", "IGRA", "FSL", "6201FB", "6201VB".
#' @param messages_path Path to messages file (default: "aermet_messages.txt").
#' @param onsite_formats List of onsite data formats (default: NULL). See  Table B-3 and B-4 of the AERMET user guide.
#' @param windspeed_zero_threshold Threshold for the minimum wind speed required to detect air flow (default: 0 m/s).
#' @param job_options,surface_options,upperair_options,onsite_prog_options,metprep_options
#'   Additional optional settings passed to each control pathway in the input file.
#'   See [aermet_job_options()], [aermet_surface_options()], [aermet_upperair_options()], [aermet_onsite_prog_options()], and [aermet_metprep_options()].
#' @param is_asos Whether or not the surface station is an ASOS station (default: FALSE).
#' @param use_onsite Whether to use an onsite or prognostic pathway for the onsite_prog option (default: TRUE).
#' @param onsite_prog_over_land Whether or not the onsite/prog station is over land (instad of over water) (default: TRUE).
#' @param expand_paths Whether to expand relative paths to absolute paths (default: TRUE).
#' @param verbose Whether to print extra messages (default: TRUE).
#' @export
make_aermet_inp <- function(
  inp_path = "aermet.inp",
  surface_station,
  instrument_heights,
  upperair_station,
  onsite_prog_station = NULL, # TODO: required? when not allowed?
  site_characteristics = data.frame(
    frequency = list(NULL, c("ANNUAL", "SEASONAL", "MONTHLY"))[[1]],
    years = list(NULL, c("2020 2021 2022", "2023", "2024 2025"))[[1]],
    sector_start = NULL,
    sector_end = NULL,
    frequency_id = NULL,
    sector_id = NULL,
    albedo = NULL,
    bowen_ratio = NULL,
    surface_roughness = NULL
  ), # SITE_CHAR(2)
  messages_path = "aermet_messages.txt",
  output_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".sfc"),
  profile_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = "_QA.pfl"),
  surface_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".iqa"),
  surface_data_format = c(
    "EXTRACT",
    "CD144",
    "SCRAM",
    "SAMSON",
    "HUSWO",
    "ISHD"
  )[1],
  upperair_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = "_upperair.txt"),
  upperair_data_format = c("EXTRACT", "IGRA", "FSL", "6201FB", "6201VB")[1],
  onsite_prog_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = "_onsite_prog.txt"),
  onsite_formats = NULL, 
  windspeed_zero_threshold = 0,
  job_options = aermet_job_options(),
  surface_options = aermet_surface_options(),
  upperair_options = aermet_upperair_options(),
  onsite_prog_options = aermet_onsite_prog_options(),
  metprep_options = aermet_metprep_options(),
  is_asos = FALSE,
  use_onsite = TRUE, # if FALSE, use prog
  onsite_prog_over_land = TRUE, # if FALSE, then over water
  expand_paths = TRUE,
  verbose = TRUE
) {
  job_options_fmtted <- list(MESSAGES = messages_path) |>
    c(job_options) |>
    format_aermet_job_options(expand_paths = expand_paths)

  surface_options_fmtted <- list(
    DATA = surface_path |>
      format_data_option(format = surface_data_format, is_asos = is_asos),
    LOCATION = surface_station |> format_location_option()
  ) |>
    c(surface_options) |>
    format_aermet_surface_options(expand_paths = expand_paths)

  upperair_options_fmtted <- list(
    DATA = upperair_path |>
      format_data_option(format = upperair_data_format),
    LOCATION = upperair_station |> format_location_option()
  ) |>
    c(upperair_options) |>
    format_aermet_upperair_options(expand_paths = expand_paths)

  onsite_ids <- if (use_onsite) seq_along(onsite_formats)
  onsite_prog_options_fmtted <- list(
    DATA = onsite_prog_path |>
      format_data_option(
        land_water = ifelse(onsite_prog_over_land, "OL", "OW")
      ),
    LOCATION = onsite_prog_station |> format_location_option(),
    # TODO: df input instead so names flexible?
    READ = if (use_onsite) paste(onsite_ids, names(onsite_formats)),
    FORMAT = if (use_onsite) paste(onsite_ids, onsite_formats),
    THRESHOLD = windspeed_zero_threshold
  ) |>
    c(onsite_prog_options) |>
    format_aermet_onsite_and_prog_options()

  sectors <- site_characteristics |>
    dplyr::filter(!duplicated(sector_id)) |>
    dplyr::select(start = sector_start, end = sector_end)

  if (!"years" %in% names(site_characteristics)) {
    site_characteristics$years <- ""
  }
  landuse_change_frequency <- site_characteristics |>
    dplyr::count(frequency, frequency_id, years) |>
    dplyr::distinct(frequency, n, years)
  freq_sector_range <- list(ANNUAL = "1", SEASONAL = "1-4", MONTHLY = "1-12")[[
    landuse_change_frequency$frequency[1]
  ]]

  fmt_int3 <- \(x) formatC(x, format = "d", digits = 0, width = 3)
  fmt_num <- \(x) formatC(x, format = "f", digits = 2, drop0trailing = FALSE)
  metprep_options_fmtted <- list(
    OUTPUT = output_path |> format_path_options(expand_paths = expand_paths),
    PROFILE = profile_path |> format_path_options(expand_paths = expand_paths),
    "**   " = "--|instrument|height (m)|--",
    NWS_HGT = "%s %s" |>
      sprintf(
        names(instrument_heights) |>
          formatC(width = max(nchar(names(instrument_heights)))),
        instrument_heights
      ),
    # TODO: handle FREQ_SECT2 SECTOR2 and SITE_CHAR2
    "**" = "Define Sectors: Time (t = %s), Space (s = 1-%s)" |>
      sprintf(freq_sector_range, nrow(sectors)),
    FREQ_SECT = landuse_change_frequency |>
      with(paste(frequency, n, years)),
    "** " = "        --|s|sta|end|-- (degrees)",
    SECTOR = with(
      sectors,
      paste(seq_along(start), fmt_int3(start), fmt_int3(end))
    ),
    "**  " = "       --|t|s|albd|bowr|srfr|--",
    SITE_CHAR = with(
      site_characteristics |> dplyr::arrange(frequency_id, sector_id),
      paste(
        frequency_id,
        sector_id,
        fmt_num(albedo),
        fmt_num(bowen_ratio),
        fmt_num(surface_roughness)
      )
    ),
    "  " = ""
  ) |>
    c(metprep_options) |>
    format_aermet_metprep_options(expand_paths = expand_paths)

  job_lines <- c("JOB", job_options_fmtted)
  surface_lines <- c("SURFACE", surface_options_fmtted)
  upperair_lines <- c("UPPERAIR", upperair_options_fmtted)
  onsite_or_prog_lines <- ifelse(use_onsite, "ONSITE", "PROG") |>
    c(onsite_prog_options_fmtted)
  metprep_lines <- c("METPREP", metprep_options_fmtted)

  # write & log
  lines <- c(
    job_lines,
    "",
    surface_lines,
    "",
    upperair_lines,
    "",
    onsite_or_prog_lines,
    "",
    metprep_lines
  )
  writeLines(lines, con = inp_path)
  if (verbose) {
    cli::cli_inform(
      "Created {.file {inp_path}}" |>
        c(lines |> gsub(pattern = "^ ", replacement = "\u00A0"))
    )
  }

  invisible(lines)
}
