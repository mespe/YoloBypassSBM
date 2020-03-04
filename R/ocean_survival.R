#' Draw number of adults returning from ocean
#'
#' Draw a random number of adults returning from ocean after 3 years based on fork length
#'
#' @md
#' @param fork_length   Fork length (mm) at estuary entry (Chipps Island)
#'
#' @export
#' @examples
#' ocean_survival(100)
#'

ocean_survival <- function(fork_length, stochastic = TRUE){
  stand_dev <- 1.430318
  if (stochastic == FALSE) stand_dev <- 0
  x <- exp(rnorm(n = 1, mean = -7.489501 + 0.023172 * fork_length, sd = stand_dev))
  ifelse(x < 0, 0, ifelse(x > 1, 1, x))
}
