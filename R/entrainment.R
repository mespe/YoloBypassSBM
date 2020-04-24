#' Fish entrainment at Fremont Weir
#'
#' Draw random number of fish entrained at Fremont Weir based on scenario, model day, and abundance at Fremont Weir
#'
#' @md
#'
#' @param model_day        Day that cohort reaches Fremont Weir
#' @param abundance        Abundance of cohort at Fremont Weir
#' @param scenario         Scenario: Exg, Alt01, Alt04b, Alt04, Alt05, Alt06
#' @param sim_type         Simulation type: deterministic or stochastic
#' @param proportion_list  Proportion entrained at Fremont Weir on each day for each scenario
#'
#'
#' @export
#'

entrainment <- function(model_day, abundance, scenario = c("Exg", "Alt01", "Alt04b", "Alt04", "Alt05", "Alt06"),
                        sim_type = c("deterministic", "stochastic"), proportion_list = fremont_weir_proportion){
  sim_type <- match.arg(sim_type)
  scenario <- match.arg(scenario)

  if(length(model_day) != length(abundance))
    stop("model_day and abundance must be the same length")

  proportion <- proportion_list[[scenario]][["Value"]][model_day]

  if (sim_type == "stochastic") {
    entrained <- mapply(function(abun, prop) rbinom(n = 1, size = abun, prob = prop),
                          round(abundance), proportion)
  } else {
    entrained <- proportion * abundance
  }
  list("Yolo" = entrained, "Sac" = abundance - entrained)
}
