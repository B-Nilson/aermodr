# TODO: document
estimate_surface_characteristics <- function(
  data = NULL,
  landuse_types = c(
    "all",
    "Water (fresh & salt)",
    "Deciduous Forest",
    "Coniferous Forest",
    "Swamp",
    "Cultivated Land",
    "Grassland",
    "Urban",
    "Desert Shrubland"
  )[1],
  characteristics = c("all", "albedo", "bowen_ratio", "surface_roughness")[1],
  seasons = c("all", "Spring", "Summer", "Fall", "Winter")[1],
  moisture_conditions = c("dry", "average", "wet")[1],
  join_by = c("landuse_type", "season", "moisture_condition")
) {
  all_characteristics <- get_surface_characteristic_estimates()

  if (!is.null(data)) {
    data |>
      dplyr::left_join(all_characteristics, by = join_by) |>
      tidyr::pivot_wider(
        names_from = "characteristic",
        values_from = "value"
      ) |>
      dplyr::left_join(
        all_characteristics |>
          dplyr::filter(is.na(.data$moisture_condition)) |>
          dplyr::select(
            dplyr::all_of(join_by[-3]),
            "characteristic",
            "value"
          ) |>
          tidyr::pivot_wider(
            names_from = "characteristic",
            values_from = "value"
          ),
        by = join_by[-3]
      )
  } else {
    all_characteristics |>
      dplyr::filter(
        .data$season %in% seasons | seasons == "all",
        .data$landuse_type %in% landuse_types | landuse_types == "all",
        .data$moisture_condition %in%
          moisture_conditions |
          is.na(.data$moisture_condition),
        .data$characteristic %in% characteristics | characteristics == "all"
      ) |>
      dplyr::select(-"moisture_condition") |>
      tidyr::pivot_wider(
        id_cols = c("landuse_type", "season"),
        names_from = "characteristic",
        values_from = "value"
      )
  }
}

get_surface_characteristic_estimates <- function() {
  landuse_types <- c(
    "Water (fresh & salt)",
    "Deciduous Forest",
    "Coniferous Forest",
    "Swamp",
    "Cultivated Land",
    "Grassland",
    "Urban",
    "Desert Shrubland"
  )

  ## Source: https://gaftp.epa.gov/Air/aqmg/SCRAM/models/met/aermet/aermet_userguide.pdf
  # Table 3-1. Albedo of Ground Covers by Land Use and Season
  albedo <- data.frame(
    landuse_type = landuse_types,
    Spring = c(0.12, 0.12, 0.12, 0.12, 0.14, 0.18, 0.14, 0.30),
    Summer = c(0.10, 0.12, 0.12, 0.14, 0.20, 0.18, 0.16, 0.28),
    Fall = c(0.14, 0.12, 0.12, 0.16, 0.18, 0.20, 0.18, 0.28),
    Winter = c(0.20, 0.50, 0.35, 0.30, 0.60, 0.60, 0.35, 0.45)
  )
  bowen_ratio <- list(
    # Table 3-2. Daytime Bowen Ratio by Land Use and Season – Dry Conditions
    dry = data.frame(
      landuse_type = landuse_types,
      Spring = c(0.1, 1.5, 1.5, 0.2, 1.0, 1.0, 2.0, 5.0),
      Summer = c(0.1, 0.6, 0.6, 0.2, 1.5, 2.0, 4.0, 6.0),
      Fall = c(0.1, 2.0, 1.5, 0.2, 2.0, 2.0, 4.0, 10.0),
      Winter = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 10.0)
    ),
    # Table 3-3. Daytime Bowen Ration by Land Use and Season - Average Moisture Conditions
    average = data.frame(
      landuse_type = landuse_types,
      Spring = c(0.1, 0.7, 0.7, 0.1, 0.3, 0.4, 1.0, 3.0),
      Summer = c(0.1, 0.3, 0.3, 0.1, 0.5, 0.8, 2.0, 4.0),
      Fall = c(0.1, 1.0, 0.8, 0.1, 0.7, 1.0, 2.0, 6.0),
      Winter = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 6.0)
    ),
    # Table 3-4. Daytime Bowen Ratio by Land Use and Season - Wet Conditions
    wet = data.frame(
      landuse_type = landuse_types,
      Spring = c(0.1, 0.3, 0.3, 0.1, 0.2, 0.3, 0.5, 1.0),
      Summer = c(0.1, 0.2, 0.2, 0.1, 0.3, 0.4, 1.0, 1.5),
      Fall = c(0.1, 0.4, 0.3, 0.1, 0.4, 0.5, 1.0, 2.0),
      Winter = c(0.3, 0.5, 0.3, 0.5, 0.5, 0.5, 0.5, 2.0)
    )
  ) |>
    dplyr::bind_rows(.id = "moisture_condition")
  # Table 3-5. Surface Roughness Length, in Meters, by Land Use and Season
  surface_roughness <- data.frame(
    landuse_type = landuse_types,
    Spring = c(0.0001, 1.00, 1.30, 0.20, 0.03, 0.05, 1.00, 0.30),
    Summer = c(0.0001, 1.30, 1.30, 0.20, 0.20, 0.10, 1.00, 0.30),
    Fall = c(0.0001, 0.80, 1.30, 0.20, 0.05, 0.01, 1.00, 0.30),
    Winter = c(0.0001, 0.50, 1.30, 0.05, 0.01, 0.001, 1.00, 0.15)
  )

  list(
    albedo = albedo,
    bowen_ratio = bowen_ratio,
    surface_roughness = surface_roughness
  ) |>
    lapply(
      tidyr::pivot_longer,
      cols = -c("landuse_type", dplyr::any_of("moisture_condition")),
      names_to = "season"
    ) |>
    dplyr::bind_rows(.id = "characteristic")
}
