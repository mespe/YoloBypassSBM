#' Number of adults returning from ocean
#'
#' Number of adults returning from ocean after three years
#'
#' @md
#' @param abundance        Abundance of cohort at esturary entry
#' @param fork_length      Average fork length (mm) of cohort at estuary entry (Chipps Island)
#' @param ocean_year_type  Type of ocean survival relationship used for that year: length or intercept
#' @param sim_type         Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

ocean_survival <- function(abundance, fork_length,
                           ocean_year_type = c("length", "intercept"), sim_type){

  ocean_year_type = match.arg(ocean_year_type)

  if(length(fork_length) != length(abundance))
    stop("fork_length and abundance must be the same length")

  # slope = 0 in parameters for intercept-only model
  params <- ocean_survival_parameters[[ocean_year_type]]

  survival <- inv_logit(params[["inter"]] + params[["slope"]] * fork_length)

  if (sim_type == "stochastic") {
    returning_adults <- mapply(function(abun, surv) VGAM::rbetabinom(1, size = abun, prob = surv, rho = params[["phi"]]),
                               round(abundance), survival)
  } else {
    returning_adults <- survival * abundance
  }
  returning_adults
}
