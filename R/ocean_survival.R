#' Number of adults returning from ocean
#'
#' Number of adults returning from ocean after three years based on weight
#'
#' @md
#' @param predictor     Predictor use in ocean survival relationship: length or weight
#' @param fork_length   Average fork length (mm) of cohort at estuary entry (Chipps Island)
#' @param wet_weight    Average wet weight (g) of cohort at estuary entry (Chipps Island)
#' @param abundance     Abundance of cohort at esturary entry
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

ocean_survival <- function(predictor = c("length", "weight"), fork_length, wet_weight, abundance, sim_type){

  predictor <- match.arg(predictor)
  predvar <- if(predictor == "length") fork_length else wet_weight

  if(length(predvar) != length(abundance))
    stop("predictor variable and abundance must be the same length")

  params <- ocean_survival_parameters[[predictor]]

  survival <- inv_logit(params[["inter"]] + params[["slope"]] * predvar)

  if (sim_type == "stochastic") {
    returning_adults <- mapply(function(abun, surv) VGAM::rbetabinom(1, size = abun, prob = surv, rho = params[["phi"]]),
                               round(abundance), survival)
  } else {
    returning_adults <- survival * abundance
  }
  returning_adults
}
