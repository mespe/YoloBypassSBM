#' Rearing survival
#'
#' Rearing survival based on rearing time and daily survival
#'
#' @md
#' @param model_day     Model day at start of rearing period
#' @param abundance     Abundance at start of rearing period
#' @param duration      Number of days spent rearing
#' @param location      Rearing location: Yolo or Delta
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'

rearing_survival <- function(model_day, abundance, duration, location = c("Delta", "Yolo"), sim_type){
  location <- match.arg(location)

  p <- rearing_survival_parameters[[location]]

  if (location == "Delta"){

    daily_survival <- p[["survival"]]

    if (sim_type == "stochastic") {
      daily_survival <- runif(length(duration), p[["min"]], p[["max"]])
    }

  } else {

    inundated_mean <- mapply(function(md, dur) mean(inundated_sqkm[["Value"]][md:(md + dur)]),
                             model_day, duration)

    daily_survival <- logistic(inundated_sqkm[["Value"]][model_day],
                               p[["max"]], p[["steepness"]], p[["inflection"]], p[["min"]])

    # if duration is zero, then set survival to 1 to avoid potential log(0)
    daily_survival <- ifelse(duration == 0, 1, daily_survival)

    if (sim_type == "stochastic"){
      # dividing by abundance to get stochastic daily survival that is turned into overall survival below
      daily_abundance <- mapply(function(abun, ds) rbinom(n = 1, size = abun, prob = ds),
                                round(abundance), daily_survival)
      # trying to catch problem values so we end up with zero rather than NaN
      # setting survival to 1 following same logic as above
      daily_survival <- ifelse(abundance == 0, 1, daily_abundance/abundance)
    }

  }
  abundance * exp(log(daily_survival) * duration)
}
