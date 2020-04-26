#' Initial cohort abundance
#'
#' Initial cohort abundances based on water year and Chinook run
#'
#' @md
#' @param water_year    Water year 1997-2011
#' @param chinook_run   Run timing classification: Fall, LateFall, Winter, Spring
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'

initial_cohort_abundance <- function(water_year, chinook_run, sim_type){

  abun <- annual_abundance[[chinook_run]][annual_abundance[["WaterYear"]] == water_year]
  prop <- knights_landing_timing[[chinook_run]][knights_landing_timing[["WaterYear"]] == water_year]

  if (sim_type == "stochastic") {
    daily_abun <- rmultinom(n = 1, size = abun, prob = prop)[, 1]
  } else {
    daily_abun <- prop * abun
  }
  daily_abun
}
