#' Delta rearing time
#'
#' Delta rearing time after adjusting for passage time and temperature
#'
#' @md
#' @param model_day     Model day when rearing was initiated
#' @param passage_time  Passge time
#' @param fork_length   Average fork length (mm) of cohort
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

rearing_time_delta <- function(model_day, passage_time, fork_length, sim_type){

  if(length(model_day) != length(passage_time) || length(passage_time) != length(fork_length))
    stop("model_day, passage_time, and fork_length must be the same length")

  params = rearing_time_parameters[["Delta"]]
  flow <- freeport_flow[["Value"]][model_day]
  rt_fd <- exp(params[["inter"]] + params[["flow"]] * flow + params[["fork_length"]] * fork_length)

  if (sim_type == "stochastic"){
    rt_fd <- sapply(rt_fd, function(rt) MASS::rnegbin(n = 1, mu = rt, theta = params[["theta"]]))
  }

  # subtracting passage time b/c relationship for rearing time includes passage and rearing
  rt_vals <- rt_fd - passage_time
  rt_vals <- ifelse(rt_vals < 0, 0, rt_vals)

  mapply(function(md, dur) temperature_adjustment(md, dur, "Delta"), model_day, rt_vals)
}

