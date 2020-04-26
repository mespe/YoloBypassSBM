#' Passage survival from Fremont Weir to Chipps Island
#'
#' Passage survival from Fremont Weir to Chipps Island based on fork length, flow, and route
#'
#' @md
#' @param abundance     Abundance of cohort on day route entered
#' @param fork_length   Fork length (mm) at Fremont Weir
#' @param flow          Flow (cfs) at Freeport on day route entered
#' @param route         Route: Sacramento River (Sac) or Yolo Bypass (Yolo)
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#' @examples
#' passage_survival(1000, 70, 60000, "Sac", "deterministic")
#' passage_survival(1000, 70, 60000, "Yolo", "deterministic")
#'

passage_survival <- function(abundance, fork_length, flow, route, sim_type){

  params <- telemetry_parameters

  # fork_length and flow were centered in telemetry model
  fork_length <- fork_length - params[["mean_fl"]]
  flow <- flow - params[["mean_flow"]]

  survival <- inv_logit(params[["alpha_survival"]] +
                          params[["beta_survival[1]"]] * fork_length +
                          params[["beta_survival[2]"]] * flow +
                          params[["beta_survival[3]"]] * as.integer(route == "Yolo"))

  if (sim_type == "stochastic") {
    chipps_num <- mapply(function(abun, surv) rbinom(n = 1, size = abun, prob = surv),
                         round(abundance), survival)
  } else {
    chipps_num <- survival * abundance
  }
  chipps_num
}
