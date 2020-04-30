#' Number of adults returning from ocean
#'
#' Number of adults returning from ocean after three years based on fork length
#'
#' @md
#' @param abundance     Abundance of cohort at esturary entry
#' @param fork_length   Average fork length (mm) of cohort at estuary entry (Chipps Island)
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

ocean_survival <- function(abundance, fork_length, sim_type){

  if(length(fork_length) != length(abundance))
    stop("fork_length and abundance must be the same length")

  params <- ocean_survival_parameters

  survival <- inv_logit(params[["inter"]] + params[["slope"]] * fork_length)

  if (sim_type == "stochastic") {
    returning_adults <- mapply(function(abun, surv) VGAM::rbetabinom(1, size = abun, prob = surv, rho = params[["phi"]]),
                               round(abundance), survival)
  } else {
    returning_adults <- survival * abundance
  }
  returning_adults
}
