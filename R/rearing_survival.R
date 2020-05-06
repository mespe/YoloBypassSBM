#' Rearing survival
#'
#' Rearing survival based on rearing time and daily survival
#'
#' @md
#' @param model_day     Model day at start of rearing period
#' @param abundance     Abundance at start of rearing period
#' @param rearing_time  Number of days spent rearing
#' @param location      Rearing location: Yolo or Delta
#' @param scenario      Scenario: Exg, Alt01, Alt04b, Alt04, Alt05, Alt06
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'

rearing_survival <- function(model_day, abundance, rearing_time, location = c("Delta", "Yolo"), scenario, sim_type){
  location <- match.arg(location)

  p <- rearing_survival_parameters[[location]]

  if (location == "Delta"){

    if (sim_type == "stochastic") {
      daily_survival <- runif(length(rearing_time), p[["min"]], p[["max"]])
    } else {
      daily_survival <- p[["survival"]]
    }
    post_rearing_abundance <- abundance * exp(log(daily_survival)*rearing_time)

  } else {

    proportion <- logistic(inundated_sqkm[[scenario]][["Value"]][model_day],
                           p[["max"]], p[["steepness"]], p[["inflection"]], p[["min"]])
    if (sim_type == "stochastic"){
      post_rearing_abundance <- mapply(function(abun, prop) rbinom(n = 1, size = abun, prob = prop),
                                       round(abundance), proportion)
    } else {
      post_rearing_abundance <- abundance * proportion
    }
  }
  post_rearing_abundance
}
