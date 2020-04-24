#' Rearing time proportion
#'
#' Proportion of drawn rearing time that cohort *potentially* rears on the Yolo Bypass based on hypothetical logistic function;
#' actual rearing time might be truncated by temperature thresholds
#'
#' @md
#' @param fork_length   Fork length (mm) at Fremont Weir
#' @param params        Parameters for four-parameter logistic function
#'
#' @export
#' @examples
#' rearing_time_proportion(70)
#' rearing_time_proportion(150)
#'

rearing_time_proportion <- function(fork_length, params = rearing_probability_parameters){
  params[["min"]] + (params[["max"]] - params[["min"]])/(1 + (fork_length / params[["inflection"]])^params[["steepness"]])
}

