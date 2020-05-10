#' Fish entrainment at Fremont Weir
#'
#' Draw random number of fish entrained at Fremont Weir based on model day and abundance at Fremont Weir
#'
#' @md
#'
#' @param model_day        Day that cohort reaches Fremont Weir
#' @param abundance        Abundance of cohort at Fremont Weir
#' @param sim_type         Simulation type: deterministic or stochastic
#'
#'
#' @export
#'

entrainment <- function(model_day, abundance, sim_type){

  if(length(model_day) != length(abundance))
    stop("model_day and abundance must be the same length")

  proportion <- fremont_weir_proportion[["Value"]][model_day]

  if (sim_type == "stochastic") {
    entrained <- mapply(function(abun, prop) rbinom(n = 1, size = abun, prob = prop),
                        round(abundance), proportion)
  } else {
    entrained <- proportion * abundance
  }
  list("Yolo" = entrained, "Sac" = abundance - entrained)
}
