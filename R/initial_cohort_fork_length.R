#' Draw initial cohort fork length
#'
#' Draw initial cohort for fork length based on run timing and model day
#'
#' @md
#' @param model_day     Day that cohort enters the model
#' @param run           Run timing classification: Fall, LateFall, Winter, Spring
#' @param sim_type      Simulation type: deterministic or stochastic
#' @param params        Knights Landing fork length parameters
#'
#'
#' @export
#'

initial_cohort_fork_length <- function(model_day, run = c("Fall", "LateFall", "Winter", "Spring"),
                                       sim_type = c("deterministic", "stochastic"), params = knights_landing_fl_params){
  run <- match.arg(run)
  sim_type <- match.arg(sim_type)

  if (length(run) > 1 || length(sim_type) > 1)
    stop("run and sim_type must have length = 1")

  if (sim_type == "stochastic") {
    fork_length <- mapply(function(fl, sd) rlnorm(1, fl, sd),
                         params[[run]][["MeanLog"]][model_day],
                         params[[run]][["SDLog"]][model_day])
  } else {
    fork_length <- exp(params[[run]][["MeanLog"]][model_day])
  }
  fork_length
}
