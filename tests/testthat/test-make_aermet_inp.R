test_that("test case works", {
  date_range <- c(as.Date("2020-01-01"), as.Date("2022-12-31"))
  instruments <- list(
    surface = data.frame(
      name = c("TEMP", "RH"),
      height = c(10, 10),
      min = c(-60, 0),
      max = c(60, 100),
      symbol = c("<=", "<="),
      missing_indicator = c(-999, -999),
      no_missing = c(FALSE, TRUE)
    ),
    upperair = data.frame(
      name = c("TEMP", "RH"),
      min = c(-80, 0),
      max = c(60, 100),
      symbol = c("<=", "<="),
      missing_indicator = c(-999, -999),
      no_missing = c(FALSE, TRUE)
    )
  )
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

  # Assume characteristics vary seasonally and East-West of the location
  wind_sector_step <- 180
  site_characteristics <- data.frame(
    frequency = "SEASONAL",
    season = c("Spring", "Summer", "Fall", "Winter") |> rep(each = 2), # not required, but used for estimate_surface_characteristics()
    sector_start = seq(0, 359.9, by = wind_sector_step),
    sector_end = seq(wind_sector_step, 360, by = wind_sector_step),
    frequency_id = rep(1:4, each = 2),
    sector_id = rep(1:2, times = 4),
    landuse_type = rep(c("Urban", "Grassland"), times = 4), # not required, but used for estimate_surface_characteristics()
    moisture_condition = rep(c("dry", "wet"), times = 4) # not required, but used for estimate_surface_characteristics()
  ) |>
    estimate_surface_characteristics()

  temp <- file.path(tempdir(), "aermet.inp")
  on.exit(unlink(dirname(temp), recursive = TRUE))
  result <- make_aermet_inp(
    inp_path = temp,
    surface_station = surface_station,
    upperair_station = upperair_station,
    onsite_prog_station = onsite_prog_station,
    instrument_heights = instruments$surface$height |> 
      stats::setNames(instruments$surface$name),
    site_characteristics = site_characteristics,
    onsite_formats = list("TEMP" = "FREE", "RH" = "FREE"),
    job_options = aermet_job_options(
      REPORT = "aermet_report.log",
      DEBUG = "aermet_debug.log",
      CHK_SYNTAX = TRUE
    ),
    surface_options = aermet_surface_options(
      EXTRACT = "surface.csv",
      XDATES = date_range,
      QAOUT = "surface.qa",
      AUDIT = "ALL",
      RANGE = instruments$surface,
      NO_MISSING = instruments$surface$name[instruments$surface$no_missing],
      ASOS1MIN = "asos1min.csv"
    ),
    upperair_options = aermet_upperair_options(
      EXTRACT = "upperair.csv",
      XDATES = date_range,
      QAOUT = "upperair.qa",
      AUDIT = "ALL",
      RANGE = instruments$upperair,
      NO_MISSING = instruments$upperair$name[instruments$upperair$no_missing],
      MODIFY = "ALL"
    ),
    expand_paths = FALSE,
    verbose = FALSE
  ) |>
    expect_silent()
  result |> paste0(collapse = "\n") |> cat() |> expect_snapshot()
})
