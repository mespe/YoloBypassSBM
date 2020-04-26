#' Passage time from Fremont Weir to Chipps
#'
#' Passage time (days) from Fremont Weir to Chipps Island based on fork length, flow, and route
#'
#' @md
#' @param fork_length   Fork length (mm) at Fremont Weir
#' @param flow          Flow (cfs) at Freeport on day route entered
#' @param route         Route: Sacramento River (Sac) or Yolo Bypass (Yolo)
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#' @examples
#' passage_time(70, 60000, "Sac", "deterministic")
#' passage_time(70, 60000, "Yolo", "deterministic")
#'

passage_time <- function(fork_length, flow, route, sim_type){

  params <- telemetry_parameters

  # fork_length and flow were centered in telemetry model
  fork_length <- fork_length - params[["mean_fl"]]
  flow <- flow - params[["mean_flow"]]

  travel_time_mu <- params[["alpha_travel"]] + params[["beta_travel[1]"]] * fork_length +
    params[["beta_travel[2]"]] * flow + params[["beta_travel[3]"]] * as.integer(route == "Yolo")

  if (sim_type == "stochastic")
    travel_time_mu <- sapply(travel_time_mu, function(x) rnorm(1, x, params[["sigma_travel"]]))

  travel_time <- exp(travel_time_mu)

  ifelse(travel_time < 0, 0, travel_time) # small chance that travel time could be negative
}



