build_receptor_lines <- function(
  receptors,
  receptor_files,
  receptor_elev_unit = "METERS",
  expand_paths = TRUE
) {
  receptor_lines <- c(
    "RE STARTING",
    if ("elev" %in% names(receptors)) {
      "   ELEVUNIT  %s" |> sprintf(toupper(receptor_elev_unit))
    },
    receptors |> build_inp_receptors()
  )
  if (!is.null(receptor_files)) {
    receptor_lines <- c(
      receptor_lines,
      "   INCLUDED  %s" |>
        sprintf(safe_path(receptor_files, expand_paths = expand_paths))
    )
  }
  receptor_lines |> c("RE FINISHED", "")
}

build_inp_receptors <- function(receptors) {
  stopifnot(
    all(names(receptors) %in% c("id", "type", "subtype", "x", "y", "z")), # TODO: add other args for each type
    nrow(as.data.frame(receptors)) > 0,
    all(
      receptors$type %in%
        c("GRIDCART", "GRIDPOLR", "DISCCART", "DISCPOLR", "EVALCART")
    ),
    all(
      receptors$subtype %in%
        c(
          "XYINC",
          "XYPNTS",
          "ELEV",
          "FLAG",
          "ORIG",
          "DIST",
          "DDIR",
          "GDIR"
        )
    ),
    !anyNA(receptors)
  )

  potential_cols <- c(
    # GRIDCART
    "xmin",
    "ymin",
    "xcount",
    "ycount",
    "xdelta",
    "ydelta",
    "x",
    "y",
    "elev",
    "flag",
    # GRIDPOLR
    "srcid",
    "ringdists",
    "ddirs",
    "dirmin",
    "dircount",
    "dirdelta",
    # DISCPOLR
    "dist",
    "dir",
    # EVALCART
    "arcid",
    "name"
  )
  receptors[potential_cols[
    !potential_cols %in% names(receptors)
  ]] <- NA_character_ # ensure all columns are present for case_when test to work

  as.data.frame(receptors) |>
    dplyr::mutate(
      entries = dplyr::case_when(
        type == "GRIDCART" ~ build_gridcart_receptors(receptors),
        type == "GRIDPOLR" ~ build_gridpolr_receptors(receptors),
        type == "DISCCART" ~ build_disccart_receptors(receptors),
        type == "DISCPOLR" ~ build_discpolr_receptors(receptors),
        type == "EVALCART" ~ build_evalcart_receptors(receptors)
      )
    ) |>
    dplyr::pull(entries)
}

build_gridcart_receptors <- function(receptors) {
  gridcarts <- receptors |>
    dplyr::filter(toupper(type) == "GRIDCART")
  if (nrow(gridcarts) == 0) {
    return(NA_character_)
  }

  gridcarts <- gridcarts |>
    tidyr::nest(.by = id) |>
    dplyr::mutate(
      entries = data |>
        sapply(\(entry) {
          dplyr::case_when(
            entry$subtype == "XYINC" ~
              "XYINC %s %s %s %s" |>
              sprintf(
                entry$xmin,
                entry$xcount,
                entry$xdelta,
                entry$ymin,
                entry$ycount,
                entry$ydelta
              ),
            entry$subtype == "XYPNTS" ~
              "XPNTS %s\n               YPNTS %s" |>
              sprintf(entry$xpoints, entry$ypoints),
            entry$subtype == "ELEV" ~
              "ELEV %s %s" |> sprintf(seq_along(entry$elev), entry$elev),
            entry$subtype == "FLAG" ~
              "FLAG %s %s" |> sprintf(seq_along(entry$flag), entry$flag)
          ) |>
            paste(collapse = "\n               ")
        })
    )

  paste(
    sep = "\n",
    "   GRIDCART  %s STA",
    "               %s",
    "   GRIDCART  %s END",
  ) |>
    sprintf(gridcarts$id, gridcarts$entries)
}

build_gridpolr_receptors <- function(receptors) {
  gridpolrs <- receptors |>
    dplyr::filter(toupper(type) == "GRIDPOLR")
  if (nrow(gridpolrs) == 0) {
    return(NA_character_)
  }

  gridpolrs <- gridpolrs |>
    tidyr::nest(.by = id) |>
    dplyr::mutate(
      entries = data |>
        sapply(\(entry) {
          dplyr::case_when(
            entry$subtype == "ORIG" ~
              "ORIG %s %s %s" |> sprintf(entry$xmin, entry$ymin, entry$srcid),
            entry$subtype == "DIST" ~
              "DIST %s" |> sprintf(entry$ringdists),
            entry$subtype == "DDIR" ~
              "DDIR " |> sprintf(entry$ddirs),
            entry$subtype == "GDIR" ~
              "GDIR %s %s %s" |>
              sprintf(entrt$dircount, entry$dirmin, entry$dirdelta),
            entry$subtype == "ELEV" ~
              "ELEV %s %s" |> sprintf(seq_along(entry$elev), entry$elev),
            entry$subtype == "FLAG" ~
              "FLAG %s %s" |> sprintf(seq_along(entry$flag), entry$flag)
          ) |>
            paste(collapse = "\n               ")
        })
    )

  paste(
    sep = "\n",
    "   GRIDPOLR %s STA",
    "               %s",
    "   GRIDPOLR %s END",
  ) |>
    sprintf(gridpolrs$id, gridpolrs$entries)
}

build_disccart_receptors <- function(receptors) {
  disccarts <- receptors |>
    dplyr::filter(toupper(type) == "DISCCART")
  if (nrow(disccarts) == 0) {
    return(NA_character_)
  }

  disccarts |>
    dplyr::mutate(
      entries = "   DISCCART  %s %s %s %s" |>
        sprintf(
          .data$x,
          .data$y,
          .data$elev |> dplyr::replace_values(NA ~ ""),
          ifelse(!is.na(.data$elev), .data$flag, "") |>
            dplyr::replace_values(NA ~ "")
        )
    ) |>
    dplyr::pull(entries)
}

build_discpolr_receptors <- function(receptors) {
  discpolrs <- receptors |>
    dplyr::filter(toupper(type) == "DISCPOLR")
  if (nrow(discpolrs) == 0) {
    return(NA_character_)
  }

  discpolrs |>
    dplyr::mutate(
      entries = "   DISCPOLR  %s %s %s %s %s" |>
        sprintf(
          .data$srcid,
          .data$dist,
          .data$dir,
          .data$elev |> dplyr::replace_values(NA ~ ""),
          ifelse(!is.na(.data$elev), .data$flag, "") |>
            dplyr::replace_values(NA ~ "")
        )
    ) |>
    dplyr::pull(entries)
}

build_evalcart_receptors <- function(receptors) {
  evalcarts <- receptors |>
    dplyr::filter(toupper(type) == "EVALCART")
  if (nrow(evalcarts) == 0) {
    return(NA_character_)
  }

  evalcarts |>
    dplyr::mutate(
      entries = "   EVALCART  %s %s %s %s" |>
        sprintf(
          .data$x,
          .data$y,
          .data$elev,
          .data$flag,
          .data$arcid,
          .data$name |> dplyr::replace_values(NA ~ "")
        )
    ) |>
    dplyr::pull(entries)
}
