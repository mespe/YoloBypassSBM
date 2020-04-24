#' Floodplain growth
#'
#' Returns final wet_weight given initial wet_weight and mean temperture during growth period; uses parameters from Perry et al. 2015
#'
#' @md
#' @param wet_weight    Wet weight (g) at start of rearing period
#' @param temp          Mean temperature (ºC) during the rearing period
#' @param duration      Duration (days) of rearing period
#' @param params        Parameters for the Ratkowsky growth model
#'
#' @export
#' @examples
#' floodplain_growth(10, 2, 20)
#' floodplain_growth(10, 20, 20)
#'

floodplain_growth <- function(wet_weight, temp, duration, params = floodplain_growth_parameters){
  if(length(wet_weight) != length(temp) || length(temp) != length(duration))
    stop("wet_weight, temp, and duration must be the same length")

  p <- params
  omega <- p[["d"]] * (temp - p[["TL"]]) * (1 - exp(p[["g"]] * (temp - p[["TU"]])))
  (wet_weight^p[["b"]] + (omega * p[["b"]] * duration)/100)^(1/p[["b"]])
}
