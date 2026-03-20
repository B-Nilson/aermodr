# TODO: ensure all digits/formats correct for sprintf entries
build_aermap_source_pathway <- function(
  sources = NULL,
  source_files = NULL,
  expand_paths = TRUE
) {
  has_sources <- !is.null(sources) | !is.null(source_files)
  if (!has_sources) {
    return(NULL)
  }

  source_lines <- c("SO STARTING")
  if (!is.null(sources)) {
    source_lines <- source_lines |>
      c(sources |> build_inp_source_locations())
  }
  if (!is.null(source_files)) {
    files_fmtted <- source_files |>
      format_path_options(expand_paths = expand_paths, collapse = FALSE)
    source_lines <- source_lines |>
      c("   INCLUDED  %s" |> sprintf(files_fmtted))
  }
  source_lines |> c("SO FINISHED", "")
}

build_inp_source_locations <- function(sources, source_lines = character(0)) {
  stopifnot(
    all(names(sources) %in% c("id", "type", "x", "y", "elev")),
    nrow(as.data.frame(sources)) > 0,
    all(sources$type %in% c("POINT", "VOLUME", "AREA", "AREAPOLY", "AREACIRC")),
    !anyNA(sources)
  )
  fmt_num <- \(x) formatC(x, format = "f", digits = 3, drop0trailing = TRUE)

  sources <- as.data.frame(sources)
  # TODO: de-loop this?
  for (i in seq_len(nrow(sources))) {
    s <- sources[i, ]
    if (is.null(s$elev)) {
      s$elev <- ""
    }
    source_lines <- c(
      source_lines,
      "   LOCATION  %s %s %s %s %s" |>
        sprintf(s$id, s$type, fmt_num(s$x), fmt_num(s$y), fmt_num(s$elev))
    )
  }
  return(source_lines)
}
