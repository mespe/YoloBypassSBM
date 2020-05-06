#' Passage survival and time from Fremont Weir to Chipps Island
#'
#' Passage survival and time (days) from Fremont Weir to Chipps Island based on fork length, flow, and route
#'
#' @md
#' @param model_day     Model day at start of passage
#' @param abundance     Abundance of cohort on day route entered
#' @param fork_length   Fork length (mm) at Fremont Weir
#' @param route         Route: Sacramento River (Sac) or Yolo Bypass (Yolo)
#' @param scenario      Scenario: Exg, Alt01, Alt04b, Alt04, Alt05, Alt06
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

passage <- function(model_day, abundance, fork_length, route = c("Sac", "Yolo"), scenario, sim_type){

  route <- match.arg(route)

  if(length(model_day) != length(abundance) || length(abundance) != length(fork_length))
    stop("model_day, abundance, and fork_length must be the same length")

  flow <- freeport_flow[[scenario]][["Value"]][model_day]
  list("Abundance" = passage_survival(abundance, fork_length, flow, route, sim_type),
       "PassageTime" = passage_time(fork_length, flow, route, sim_type))
}



