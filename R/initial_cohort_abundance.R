#' Draw initial cohort abundance
#'
#' Draw a random vector of initial cohort abundances based on run timing, water year, and annual abundance
#'
#' @md
#' @param run           Run timing classification: Fall, LateFall, Winter, Spring
#' @param water_year    Water year 1997-2011
#'
#' @export
#' @examples
#'

initial_cohort_abundance <- function(run = c("Fall", "LateFall", "Winter", "Spring"), water_year){
  run <- match.arg(run)
  abun <- annual_abundance[[run]][annual_abundance[["WaterYear"]] == water_year]
  prop <- knights_landing_timing[[run]][knights_landing_timing[["WaterYear"]] == water_year]
  rmultinom(n = 1, size = abun, prob = prop)[, 1]
}
