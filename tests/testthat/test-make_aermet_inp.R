test_that("test case works", {
  instrument_heights <- c(A = 10, B = 10, C = 10, D = 10, E = 10, F = 10)
  surface_station <- list(
    site_id = "123456",
    lat = 50.123,
    lng = -175.123,
    tz_offset = 7,
    elev = 10
  )
  upperair_station <- list(
    site_id = "654321",
    lat = 50.234,
    lng = -175.234,
    tz_offset = 7
  )
  onsite_prog_station <- list(
    site_id = "234567",
    lat = 50.012,
    lng = -175.012,
    tz_offset = 7,
    elev = 10
  )
  wind_sectors <- data.frame(
    start = seq(0, 360, by = 45),
    end = seq(45, 360 + 45, by = 45) %% 360
  )
  landuse_change_frequency <- list(
    frequency = "ANNUAL",
    n_sectors = 1,
    years = "2020 2021 2022"
  )
  site_characteristics <- data.frame(
    landuse_change_frequency_id = 1,
    landuse_change_sector_id = 1,
    albedo = 0.2,
    bowen_ratio = 4,
    surface_roughness = 1
  )

  temp <- file.path(tempdir(), "aermet.inp")
  on.exit(unlink(dirname(temp), recursive = TRUE))
  result <- make_aermet_inp(
    inp_path = temp,
    surface_station = surface_station,
    upperair_station = upperair_station,
    onsite_prog_station = onsite_prog_station,
    instrument_heights = instrument_heights,
    wind_sectors = wind_sectors, # TODO: FIX THESE BASED ON CONVO WITH LLM
    landuse_change_frequency = landuse_change_frequency,
    site_characteristics = site_characteristics,
    onsite_formats = list("TEMP" = "FREE"),
    job_options = aermet_job_options(
      REPORT = "report.aermet",
      DEBUG = "debug.aermet",
      CHK_SYNTAX = TRUE,
      NOPRINT = TRUE
    ),
    surface_options = aermet_surface_options(
      EXTRACT = "surface.csv",
      XDATES = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
      QAOUT = "surface.qa",
      AUDIT = "ALL",
      RANGE = data.frame(
        name = "TEMP",
        min = -30,
        max = 50,
        symbol = "<=",
        missing_indicator = -999
      ),
      NO_MISSING = "TEMP",
      ASOS1MIN = "asos1min.csv"
    ),
    upperair_options = aermap_upperair_options(
      EXTRACT = "upperair.csv",
      XDATES = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
      QAOUT = "surface.qa",
      AUDIT = "ALL",
      RANGE = data.frame(
        name = "TEMP",
        min = -30,
        max = 50,
        symbol = "<=",
        missing_indicator = -999
      ),
      NO_MISSING = "TEMP",
      MODIFY = "ALL"
    ),
    expand_paths = FALSE,
    verbose = FALSE
  ) |>
    expect_silent()
  result |> paste0(collapse = "\n") |> cat() |> expect_snapshot()
})
