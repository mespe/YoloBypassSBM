#' Rearing status
#'
#' Rearing status for cohorts on the Yolo Bypass (0 = not rearing; 1 = rearing);
#' deterministic option is a crude approximation of underlying relationship
#'
#' @md
#' @param fork_length   Fork length (mm) at Fremont Weir
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#' @examples
#' rearing_status(70)
#' rearing_status(150)
#'

rearing_status <- function(fork_length, sim_type = c("deterministic", "stochastic")){
  sim_type <- match.arg(sim_type)

  if (length(sim_type) > 1) stop("sim_type must have length = 1")

  if (sim_type == "stochastic") {
    status <- sapply(rearing_probability(fork_length),
                     function(x) rbinom(n = 1, size = 1, prob = x))
  } else {
    status <- ifelse(fork_length > 80, 0, 1)
  }
  status
}

