#' Rearing survival
#'
#' Rearing survival based on rearing time and daily survival
#'
#' @md
#' @param rearing_time  Number of days spent rearing
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#' @examples
#' rearing_survival(10, "deterministic")
#'

rearing_survival <- function(rearing_time, sim_type){

  params <- rearing_survival_parameters
  if (sim_type == "stochastic") {
    daily_survival <- runif(length(rearing_time), params[["min"]], params[["max"]])
  } else {
    daily_survival <- params[["survival"]]
  }
  exp(log(daily_survival)*rearing_time)
}
