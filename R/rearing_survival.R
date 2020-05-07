#' Rearing survival
#'
#' Rearing survival based on rearing time and daily survival
#'
#' @md
#' @param model_day     Model day at start of rearing period
#' @param abundance     Abundance at start of rearing period
#' @param duration      Number of days spent rearing
#' @param location      Rearing location: Yolo or Delta
#' @param scenario      Scenario: Exg, Alt01, Alt04b, Alt04, Alt05, Alt06
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'

rearing_survival <- function(model_day, abundance, duration, location = c("Delta", "Yolo"), scenario, sim_type){
  location <- match.arg(location)

  p <- rearing_survival_parameters[[location]]

  if (location == "Delta"){

    daily_survival <- p[["survival"]]

    if (sim_type == "stochastic") {
      daily_survival <- runif(length(duration), p[["min"]], p[["max"]])
    }

  } else {

    daily_survival <- logistic(inundated_sqkm[[scenario]][["Value"]][model_day],
                               p[["max"]], p[["steepness"]], p[["inflection"]], p[["min"]])

    if (sim_type == "stochastic"){
      # might be confused when I come back to this
      # dividing by abundance to get stochastic daily survival
      daily_survival <- mapply(function(abun, ds) rbinom(n = 1, size = abun, prob = ds),
                               round(abundance), daily_survival)/abundance
    }
  }
  abundance * exp(log(daily_survival)*duration)
}
