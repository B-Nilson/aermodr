test_that("test case works", {
  anchor <- list(
    x_user = 0,
    y_user = 0,
    x_utm = 500000,
    y_utm = 5400000,
    zone = 10,
    datum = 0
  ) # TODO: clarify datum

  receptors <- data.frame(
    id = c("R1", "R2", "R3"),
    type = "DISCCART",
    subtype = "XYPNTS",
    x = c(0, 100, 200),
    y = c(0, 100, 200)
  )

  sources <- data.frame(
    id = c("S1", "S2"),
    type = "POINT",
    x = c(50, 150),
    y = c(50, 150),
    z = c(10, 15)
  )

  temp <- file.path(tempdir(), "aermap.inp")
  on.exit(unlink(dirname(temp), recursive = TRUE))
  result <- temp |>
    make_aermap_inp(
      anchor = anchor,
      sources = sources,
      receptors = receptors,
      expand_paths = FALSE,
      verbose = FALSE
    ) |>
    expect_silent()
  result |> paste0(collapse = "\n") |> cat() |> expect_snapshot()
})
