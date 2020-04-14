#' Rearing probability
#'
#' Rearing probability on the Yolo Bypass based on hypothetical logistic function
#'
#' @md
#' @param fork_length   Fork length (mm) at Fremont Weir
#' @param params        Parameters for four-parameter logistic function
#'
#' @export
#' @examples
#' rearing_probability(70)
#' rearing_probability(150)
#'

rearing_probability <- function(fork_length, params = rearing_probability_parameters){
  params[["min"]] + (params[["max"]] - params[["min"]])/(1 + (fork_length / params[["inflection"]])^params[["steepness"]])
}

