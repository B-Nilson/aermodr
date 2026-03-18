#' Run AERMAP model
#'
#' A wrapper for [`run_model()`] to run the AERMAP model.
#' @inheritParams run_model
#' @return A character value of the model log (invisible), or an error if the model log is not found (i.e. the model failed to run)
#' @export
run_aermap <- function(
  path = "AERMAP",
  exe_name = "aermap.exe",
  run_name = "aermap",
  inp_name = run_name |> paste0(".inp"),
  out_name = run_name |> paste0(".out"),
  log_name = run_name |> paste0(".log"),
  verbose = TRUE
) {
  run_model(
    path = path,
    model = "aermap",
    exe_name = exe_name,
    run_name = run_name,
    inp_name = inp_name,
    out_name = out_name,
    log_name = log_name,
    verbose = verbose
  )
}
