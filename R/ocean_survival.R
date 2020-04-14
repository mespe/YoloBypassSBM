#' Number of adults returning from ocean
#'
#' Number of adults returning from ocean after three years based on fork length
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

  os_mean <- -7.489501 + 0.023172 * fork_length

  if (sim_type == "stochastic") {
    survival <- sapply(fork_length, function (fl) exp(rnorm(n = 1, mean = os_mean, sd = 1.430318)))
  } else {
    survival <- exp(os_mean)
  }
  ifelse(survival < 0, 0, ifelse(survival > 1, 1, survival))
}
