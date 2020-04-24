#' Number of adults returning from ocean
#'
#' Number of adults returning from ocean after three years based on weight
#'
#' @md
#' @param wet_weight    Average wet weight (g) of cohort at estuary entry (Chipps Island)
#' @param abundance     Abundance of cohort at esturary entry
#' @param sim_type      Simulation type: deterministic or stochastic
#' @param params        Parameters from betabinomial regression of ocean recoveries versus release weight
#'
#' @export
#' @examples
#' ocean_survival(40, 1000)
#'

ocean_survival <- function(wet_weight, abundance, sim_type = c("deterministic", "stochastic"),
                           params = ocean_survival_parameters){
  sim_type <- match.arg(sim_type)

  if(length(wet_weight) != length(abundance))
    stop("wet_weight and abundance must be the same length")

  survival <- inv_logit(params[["inter"]] + params[["slope"]] * wet_weight)

  if (sim_type == "stochastic") {
    returning_adults <- mapply(function(abun, surv) VGAM::rbetabinom(1, size = abun, prob = surv, rho = params[["phi"]]),
                               round(abundance), survival)
  } else {
    returning_adults <- survival * abundance
  }
  returning_adults
}
