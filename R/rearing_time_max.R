#' Maximum floodplain rearing time
#'
#' Maximum floodplain rearing time before adjusting for fish size and temperature
#'
#' @md
#' @param flood_duration   Flood duration (days)
#' @param sim_type         Simulation type: deterministic or stochastic
#'
#' @export
#' @examples
#' rearing_time_max(10, "deterministic")
#' rearing_time_max(30, "deterministic")
#'

rearing_time_max <- function(flood_duration, sim_type){

  params = rearing_time_parameters
  rearing_time <- exp(params[["inter"]] + params[["slope"]] * flood_duration)

  if (sim_type == "stochastic")
    rearing_time <- sapply(rearing_time, function(rt) MASS::rnegbin(n = 1, mu = rt, theta = params[["theta"]]))

  rearing_time
}



