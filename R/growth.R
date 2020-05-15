#' Growth
#'
#' Returns final wet_weight given initial wet_weight and mean temperture during growth period; uses parameters from Perry et al. 2015
#'
#' @md
#' @param wet_weight    Wet weight (g) at start of rearing period
#' @param temp          Mean temperature (ºC) during the rearing period
#' @param duration      Duration (days) of rearing period
#'
#' @export
#' @examples
#' growth(10, 2, 20)
#' growth(10, 20, 20)
#'

growth <- function(wet_weight, temp, duration){
  # if(length(wet_weight) != length(temp) || length(temp) != length(duration))
  #   stop("wet_weight, temp, and duration must be the same length")

  p <- growth_parameters
  omega <- p[["d"]] * (temp - p[["TL"]]) * (1 - exp(p[["g"]] * (temp - p[["TU"]])))
  (wet_weight^p[["b"]] + (omega * p[["b"]] * duration)/100)^(1/p[["b"]])
}
