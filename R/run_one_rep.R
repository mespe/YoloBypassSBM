#' Run one replicate of the model
#'
#' Run one replicate of the model; a replicate is a water_year/chinook_run/scenario/sim_type combination
#'
#' @md
#' @param water_year    Water year (1997-2011)
#' @param chinook_run   Run timing classification: Fall, LateFall, Winter, Spring
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

run_one_rep <- function(water_year, chinook_run = c("Fall", "LateFall", "Spring", "Winter"),
                         scenario = c("Exg", "Alt01", "Alt04b", "Alt04", "Alt05", "Alt06"),
                         sim_type = c("deterministic", "stochastic")){

  chinook_run <- match.arg(chinook_run)
  scenaro <- match.arg(scenario)
  sim_type <- match.arg(sim_type)

  wy_all <- annual_abundance[["WaterYear"]]
  if (!(water_year %in% wy_all))
    stop(paste("water_year must be between", min(wy_all), "and", max(wy_all)))

  if (length(water_year) > 1)
    stop("water_year must have length = 1")
}

