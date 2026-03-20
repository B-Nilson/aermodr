#' @export
make_aermap_inp <- function(
  inp_path = "aermap.inp",
  anchor,
  terrain_data_files = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".dem"),
  terrain_data_type = c("DEM", "NED")[1],
  terrain_fill_gaps = FALSE,
  receptors = NULL,
  receptor_files = NULL,
  receptor_elev_unit = c("METERS", "FEET")[1],
  sources = NULL,
  source_files = NULL,
  title = "AERMAP Run",
  output_src_file = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".src"),
  output_rec_file = basename(inp_path) |>
    sub(pattern = "\\..*$", replacement = ".rec"),
  control_options = aermap_control_options(),
  output_options = aermap_output_options(),
  expand_paths = TRUE,
  test = FALSE,
  verbose = TRUE
) {
  stopifnot(
    !is.null(receptors) | !is.null(receptor_files),
    '`terrain_fill_gaps` can only be TRUE if `terrain_data_type` is "DEM"' = !terrain_fill_gaps |
      (terrain_data_type == "DEM" & terrain_fill_gaps)
  )

  # (CO) control details
  control_lines <- title |>
    build_aermap_control_pathway(
      anchor = anchor,
      terrain_data_files = terrain_data_files,
      terrain_data_type = terrain_data_type,
      terrain_fill_gaps = terrain_fill_gaps,
      options = control_options,
      expand_paths = expand_paths,
      test = test
    )

  # (SO) source details [optional]
  source_lines <- sources |>
    build_aermap_source_pathway(
      source_files = source_files,
      expand_paths = expand_paths
    )

  # (RE) receptor details
  receptor_lines <- receptors |>
    build_aermap_receptor_pathway(
      receptor_files = receptor_files,
      receptor_elev_unit = receptor_elev_unit,
      expand_paths = expand_paths
    )

  # OUTPUT details
  output_lines <- output_rec_file |>
    build_aermap_output_pathway(
      source_file_path = output_src_file,
      options = output_options,
      expand_paths = expand_paths
    )

  # write & log
  lines <- c(control_lines, source_lines, receptor_lines, output_lines)
  writeLines(lines, con = inp_path)
  if (verbose) {
    cli::cli_inform(
      "Created {.file {inp_path}}" |>
        c(lines |> gsub(pattern = "^ ", replacement = "\u00A0"))
    )
  }

  invisible(lines)
}

build_aermap_control_pathway <- function(
  title,
  anchor,
  terrain_data_files,
  terrain_data_type = c("DEM", "NED")[1],
  terrain_fill_gaps = FALSE,
  options = aermap_control_options(),
  test = FALSE,
  expand_paths = TRUE
) {
  data_type_line <- "   DATATYPE  %s %s" |>
    sprintf(terrain_data_type, ifelse(terrain_fill_gaps, "FILLGAPS", ""))

  data_file_line <- terrain_data_files |>
    build_datafile_lines(
      data_type = terrain_data_type, # TODO: allow plural?
      options = options,
      expand_paths = expand_paths
    )

  used_options <- c("CHECKS", "TIFFDEBUGS", "ElevUnits") # used in build_datafile_lines()
  extra_option_lines <- options[
    !names(options) %in% used_options
  ] |>
    format_aermap_control_options()
  c(
    "CO STARTING",
    "   TITLEONE  %s" |> sprintf(title),
    data_type_line,
    data_file_line,
    "   ANCHORXY  %s" |> sprintf(convert_anchor_for_inp(anchor)),
    extra_option_lines,
    "   RUNORNOT  %s" |> sprintf(ifelse(!test, "RUN", "NOT")),
    "CO FINISHED",
    ""
  )
}

build_aermap_output_pathway <- function(
  receptor_file_path,
  source_file_path,
  options = aermap_output_options(),
  expand_paths = TRUE
) {
  # prepend required receptor/source options
  options <- list(RECEPTOR = receptor_file_path, SOURCLOC = source_file_path) |>
    lapply(format_path_options, expand_paths = expand_paths) |>
    c(options)

  c(
    "OU STARTING",
    options |> format_aermap_output_options(),
    "OU FINISHED",
    ""
  )
}

build_datafile_lines <- function(
  data_files,
  data_type,
  options,
  expand_paths = TRUE
) {
  if (is.null(options$TIFFDEBUGS)) {
    options$TIFFDEBUGS <- NA_character_
  }
  if (is.null(options$CHECKS)) {
    options$CHECKS <- FALSE
  }
  check_or_tiffdebug <- dplyr::case_when(
    rep(data_type, length(data_files)) == "DEM" &
      options$CHECKS ~ "CHECK",
    rep(data_type, length(data_files)) == "NED" ~ options$TIFFDEBUGS,
    .default = ""
  ) |>
    dplyr::replace_values(NA ~ "")
  ned_elev_units <-
    (data_type == "NED" & !is.na(options$TIFFDEBUGS)) |>
    ifelse(options$ElevUnits, "")

  "   DATAFILE  %s %s %s" |>
    sprintf(
      data_files |>
        format_path_options(expand_paths = expand_paths, collapse = FALSE),
      check_or_tiffdebug,
      ned_elev_units
    )
}

convert_anchor_for_inp <- function(anchor) {
  anchor <- as.list(anchor)
  fmt_xy <- \(x) formatC(x, format = "f", digits = 0)

  "%s %s  %s %s %s %s" |>
    sprintf(
      fmt_xy(anchor$x_user),
      fmt_xy(anchor$y_use),
      fmt_xy(anchor$x_utm),
      fmt_xy(anchor$y_utm),
      anchor$zone,
      anchor$datum
    )
}
