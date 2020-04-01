#' Draw number of adults returning from ocean
#'
#' Draw a random number of adults returning from ocean after 3 years based on fork length
#'
#' @md
#' @param fork_length   Fork length (mm) at estuary entry (Chipps Island)
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#' @examples
#' ocean_survival(100)
#'

ocean_survival <- function(fork_length, sim_type = c("deterministic", "stochastic")){
  sim_type <- match.arg(sim_type)
  stand_dev <- if (sim_type == "stochastic") 1.430318 else 0
  x <- exp(rnorm(n = 1, mean = -7.489501 + 0.023172 * fork_length, sd = stand_dev))
  ifelse(x < 0, 0, ifelse(x > 1, 1, x))
}
