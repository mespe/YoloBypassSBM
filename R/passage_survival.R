#' Passage survival from Fremont Weir to Chipps Island
#'
#' Passage survival from Fremont Weir to Chipps Island based on fork length, flow, and route
#'
#' @md
#' @param fork_length   Fork length (mm) at Fremont Weir
#' @param flow          Flow (cfs) at Freeport on day route entered
#' @param abundance     Abundance of cohort on day route entered
#' @param sim_type      Simulation type: deterministic or stochastic
#' @param params        Parameters from Bayesian model of survival and travel time based on acoustic telemetry data
#'
#' @export
#' @examples
#' passage_survival(70, 60000, 100000, "Sac")
#' passage_survival(70, 60000, 100000, "Yolo")
#'

passage_survival <- function(fork_length, flow, abundance, route = c("Sac", "Yolo"),
                             sim_type = c("deterministic", "stochastic"), params = telemetry_parameters){
  route <- match.arg(route)
  sim_type <- match.arg(sim_type)

  if(length(fork_length) != length(flow))
    stop("fork_length and flow must be the same length")

  if (length(route) > 1 || length(sim_type) > 1)
    stop("route and sim_type must have length = 1")

  # fork_length and flow were centered in telemetry model
  fork_length = fork_length - params[["mean_fl"]]
  flow = flow - params[["mean_flow"]]

  survival <- inv_logit(params[["alpha_survival"]] +
                          params[["beta_survival[1]"]] * fork_length +
                          params[["beta_survival[2]"]] * flow +
                          params[["beta_survival[3]"]] * as.integer(route == "Yolo"))

  abundance <- round(abundance)
  if (sim_type == "stochastic") {
    survival <- sapply(survival, function(x)
      rbinom(n = 1, size = abundance, prob = x)/abundance)
  }
  survival
}
