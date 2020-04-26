#' Initial cohort fork length
#'
#' Initial cohort fork length based on model day and Chinook run
#'
#' @md
#' @param model_day     Day that cohort enters the model
#' @param chinook_run   Run timing classification: Fall, LateFall, Winter, Spring
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'

initial_cohort_fork_length <- function(model_day, chinook_run, sim_type){

  params = knights_landing_fl_params

  if (sim_type == "stochastic") {
    fork_length <- mapply(function(fl, sd) rlnorm(1, fl, sd),
                          params[[chinook_run]][["MeanLog"]][model_day],
                          params[[chinook_run]][["SDLog"]][model_day])
  } else {
    fork_length <- exp(params[[chinook_run]][["MeanLog"]][model_day])
  }
  fork_length
}
