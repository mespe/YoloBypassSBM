#' Rearing status
#'
#' Rearing status for cohorts on the Yolo Bypass (0 = not rearing; 1 = rearing); note: no deterministic option
#'
#' @md
#' @param fork_length   Fork length (mm) at Fremont Weir
#'
#' @export
#' @examples
#' rearing_status(70)
#' rearing_status(150)
#'

rearing_status <- function(fork_length){
  sapply(rearing_probability(fork_length), function(x) rbinom(n = 1, size = 1, prob = x))
}

