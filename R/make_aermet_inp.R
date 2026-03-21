make_aermet_inp <- function(
  inp_path = "aermet.inp",
  surface_station,
  upperair_station, # TODO: required?
  onsite_prog_station, # TODO: required? when not allowed?
  wind_sectors = data.frame(start = NULL, end = NULL), # SECTOR(2)
  landuse_change_frequency = data.frame(
    frequency = list(NULL, c("ANNUAL", "SEASONAL", "MONTHLY"))[[1]],
    n_sectors = list(NULL, 1:12)[[1]],
    years = list(NULL, c("2020 2021 2022", "2023", "2024 2025"))[[1]]
  ), # FREQ_SECT(2)
  site_characteristics = data.frame(
    landuse_change_frequency_id = NULL,
    landuse_change_sector_id = NULL, # TODO: clarify
    albedo = NULL,
    bowen_ratio = NULL,
    surface_roughness = NULL
  ), # SITE_CHAR(2)
  messages_path = "aermet_messages.txt",
  output_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".out"),
  profile_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".pfl"),
  instrument_heights,
  surface_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".sfc"),
  surface_data_format = c(
    "EXTRACT",
    "CD144",
    "SCRAM",
    "SAMSON",
    "HUSWO",
    "ISHD"
  )[1],
  upperair_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".upr"),
  upperair_data_format = c("EXTRACT", "IGRA", "FSL", "6201FB", "6201VB")[1],
  onsite_prog_path = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".ons"),
  onsite_formats = NULL, # See Table B-3 and B-4
  job_options = aermet_job_options(),
  surface_options = aermet_surface_options(),
  upperair_options = aermap_upperair_options(),
  onsite_prog_options = aermap_onsite_prog_options(),
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
    FORMAT = if (use_onsite) paste(onsite_ids, onsite_formats)
  ) |>
    c(onsite_prog_options) |>
    format_aermap_onsite_and_prog_options()

  metprep_options_fmtted <- list(
    NWS_HGT = "%s %s" |>
      sprintf(names(instrument_heights), instrument_heights),
    # TODO: handle FREQ_SECT2 SECTOR2 and SITE_CHAR2
    SECTOR = with(wind_sectors, paste(seq_along(start), start, end)),
    FREQ_SECT = landuse_change_frequency |>
      with(paste(frequency, n_sectors, years)),
    SITE_CHAR = with(
      site_characteristics,
      paste(
        landuse_change_frequency_id,
        landuse_change_sector_id,
        albedo,
        bowen_ratio,
        surface_roughness
      )
    ),
    OUTPUT = output_path |> format_path_options(expand_paths = expand_paths),
    PROFILE = profile_path |> format_path_options(expand_paths = expand_paths)
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
