#' Initial cohort abundance
#'
#' Initial cohort abundances based on run timing, water year, and annual abundance
#'
#' @md
#' @param water_year    Water year 1997-2011
#' @param run           Run timing classification: Fall, LateFall, Winter, Spring
#' @param sim_type      Simulation type: deterministic or stochastic
#' @param timing_df     Knights Landing entry timing
#' @param abundance_df  Annual abundance of outmigrants
#'
#' @export
#' @examples
#'

initial_cohort_abundance <- function(water_year, run = c("Fall", "LateFall", "Winter", "Spring"),
                                     sim_type = c("deterministic", "stochastic"), timing_df = knights_landing_timing,
                                     abundance_df = annual_abundance){
  run <- match.arg(run)
  sim_type <- match.arg(sim_type)

  wy <- abundance_df[["WaterYear"]]

  if (!(water_year %in% wy))
    stop(paste("water_year must be between", min(wy), "and", max(wy)))

  if (length(water_year) > 1 || length(run) > 1 || length(sim_type) > 1)
    stop("water_year, run, and sim_type must have length = 1")

  abun <- abundance_df[[run]][abundance_df[["WaterYear"]] == water_year]
  prop <- timing_df[[run]][timing_df[["WaterYear"]] == water_year]

  if (sim_type == "stochastic") {
    daily_abun <- rmultinom(n = 1, size = abun, prob = prop)[, 1]
  } else {
    daily_abun <- prop * abun
  }
  daily_abun
}
