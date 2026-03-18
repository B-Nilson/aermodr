#' Download and unzip AERMOD, AERMAP, or AERMET model executables
#'
#' @inheritParams check_installed
#' @inheritParams get_and_unzip
#' @param is_x32 Whether to download the 32-bit version of the model (default: FALSE)
#' @return The path to the model executable. The model will be installed in `{path}/{toupper(model)}/`.
#' @export
install_model <- function(
  path = ".",
  model = c("aermod", "aermap", "aermet")[1],
  is_x32 = FALSE,
  remove_zips = TRUE,
  verbose = TRUE,
  prompt = FALSE
) {
  stopifnot(
    "`path` must be NULL or a character vector of length 1" = path |>
      is.character() &&
      length(path) == 1 ||
      is.null(path),
    "`model` must be one of 'aermod', 'aermap', or 'aermet'" = model %in%
      c("aermod", "aermap", "aermet") &
      length(model) == 1,
    "`is_x32` must be a logical vector of length 1" = is_x32 |>
      is.logical() &&
      length(is_x32) == 1,
    "`verbose` must be a logical vector of length 1" = verbose |>
      is.logical() &&
      length(verbose) == 1
  )

  model_dir <- path |>
    file.path(toupper(model))
  already_installed <- model_dir |>
    check_installed(model = model, verbose = FALSE) |>
    handyr::on_error(.return = NULL) |>
    suppressWarnings()
  is_already_installed <- !is.null(already_installed) && already_installed$found
  if (is_already_installed) {
    if (verbose) {
      cli::cli_alert_success("{toupper(model)} is already installed")
    }
    return(invisible(already_installed$path))
  }

  model_url <- get_model_url(model = model, is_x32 = is_x32)
  local_files <- model_url |>
    get_and_unzip(
      local_dir = model_dir,
      verbose = verbose,
      remove_zips = remove_zips,
      prompt = prompt
    )

  local_files[endsWith(local_files, paste0(model, ".exe"))] # TODO: what if not windows?
}

get_model_url <- function(
  model,
  is_x32 = FALSE,
  is_windows = .Platform$OS.type == "windows"
) {
  model_categories <- list(
    aermod = "preferred",
    aermap = "related",
    aermet = "met"
  )[[model]]
  base_url <- "https://gaftp.epa.gov/Air/aqmg/SCRAM/models/%s/%s/" |>
    sprintf(model_categories, model)
  "%s/%s_%s%s.zip" |>
    sprintf(
      base_url,
      model,
      ifelse(is_windows, "exe", "source"),
      ifelse(is_x32, "-32", "")
    )
}
