#' Run AERMET model
#'
#' A wrapper for [`run_model()`] to run both stages of the AERMET model.
#' @inheritParams run_model
#' @param run_names Names of the runs (default: c("aermet1", "aermet2")) - used for default names of the input and output files for each stage.
#' @param inp_names Names of the input files (default: run_names + ".inp")
#' @param log_names Names of the log files (default: run_names + ".log")
#' @return A list of length 2 with character values of the model log for each stage (invisible), or an error if the model log is not found (i.e. the model failed to run)
#' @export
run_aermet <- function(
  path = "AERMET",
  exe_name = "aermet.exe",
  run_names = c("aermet1", "aermet2"),
  inp_names = run_names |> paste0(".inp"),
  log_names = run_names |> paste0(".log"),
  verbose = TRUE
) {
  list(
    stage1 = run_model(
      path = path,
      model = "aermet",
      exe_name = exe_name,
      run_name = run_names[1],
      inp_name = inp_names[1],
      out_name = "",
      log_name = log_names[1],
      verbose = verbose
    ),
    stage2 = run_model(
      path = path,
      model = "aermet",
      exe_name = exe_name,
      run_name = run_names[2],
      inp_name = inp_names[2],
      out_name = "",
      log_name = log_names[2],
      verbose = verbose
    )
  ) |>
    invisible()
}
