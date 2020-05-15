#' Rearing abundance
#'
#' Number of individuals in cohort entrained onto Yolo Bypass that actually rears on the Yolo Bypass based on hypothetical logistic function with fork length
#'
#'
#' @md
#' @param abundance     Abundance of cohort at Fremot Weir
#' @param fork_length   Fork length (mm) at Fremont Weir
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'

rearing_abundance <- function(abundance, fork_length, sim_type){

  # if(length(abundance) != length(fork_length))
  #   stop("abundance and fork_length must be the same length")

  p <- rearing_proportion_parameters
  proportion <- logistic(fork_length, p[["max"]], p[["steepness"]], p[["inflection"]], p[["min"]])

  if (sim_type == "stochastic"){
      rbinom(n = length(proportion),
             size = round(abundance),
             prob = proportion)
  } else {
      abundance * proportion
  }
}


