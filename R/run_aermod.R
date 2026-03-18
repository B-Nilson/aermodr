#' Run AERMOD model
#'
#' A wrapper for [`run_model()`] to run the AERMOD model.
#' @inheritParams run_model
#' @return A character value of the model log (invisible), or an error if the model log is not found (i.e. the model failed to run)
#' @export
run_aermod <- function(
  path = "AERMOD",
  exe_name = "aermod.exe",
  run_name = "aermod",
  inp_name = run_name |> paste0(".inp"),
  out_name = run_name |> paste0(".out"),
  log_name = run_name |> paste0(".log"),
  verbose = TRUE
) {
  run_model(
    path = path,
    model = "aermod",
    exe_name = exe_name,
    run_name = run_name,
    inp_name = inp_name,
    out_name = out_name,
    log_name = log_name,
    verbose = verbose
  )
}
