#' Draw travel time from Fremont Weir to Chipps
#'
#' Draw a random travel time (days) from Fremont Weir to Chipps Island based on fork length, flow, and route
#'
#' @md
#' @param fork_length   Fork length (mm) at estuary entry (Chipps Island)
#' @param flow          Flow (cfs) at Freeport on day route entered
#' @param sim_type      Simulation type: deterministic or stochastic
#' @param params        Parameters from Bayesian model of survival and travel time based on acoustic telemetry data
#'
#' @export
#' @examples
#' travel_time(70, 60000, "Sac")
#' travel_time(70, 60000, "Yolo")
#'

travel_time <- function(fork_length, flow, route = c("Sac", "Yolo"),
                           sim_type = c("deterministic", "stochastic"), params = telemetry_parameters){
  route <- match.arg(route)
  sim_type <- match.arg(sim_type)

  if(length(fork_length) != length(flow))
    stop("fork_lengths and flow must be the same length")

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

# # library(tidyverse)
# crossing(FL = c(40, 80, 120, 160, 200),
#          Flow = seq(10000, 120000, 10000)) %>%
#   mutate(Sac = travel_time(FL, Flow, "Sac"),
#          Yolo = travel_time(FL, Flow, "Yolo")) %>%
#   gather(key = Route, value = TravelTime, Sac:Yolo) %>%
#   ggplot(aes(x = Flow, y = TravelTime, col = as.factor(FL))) +
#   geom_line() +
#   scale_color_brewer(name = "FL", type = "qual", palette = "Set1") +
#   facet_wrap(~Route) +
#   theme_minimal()
# ggsave("TravelTimeVsFLAndFlow.png", width = 6, height = 3.5)
#
# crossing(FL = c(40, 80, 120, 160, 200),
#          Flow = seq(10000, 120000, 10000)) %>%
#   mutate(Sac = travel_time(FL, Flow, "Sac", "stochastic"),
#          Yolo = travel_time(FL, Flow, "Yolo", "stochastic")) %>%
#   gather(key = Route, value = TravelTime, Sac:Yolo) %>%
#   ggplot(aes(x = Flow, y = TravelTime, col = as.factor(FL))) +
#   geom_point() +
#   scale_color_brewer(name = "FL", type = "qual", palette = "Set1") +
#   facet_wrap(~Route) +
#   theme_minimal()


