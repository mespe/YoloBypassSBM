#' Floodplain growth
#'
#' Returns final mass given initial mass and mean temperture during growth period; uses parameters from Perry et al. 2015
#'
#' @md
#' @param mass          Mass (g) at start of rearing period
#' @param temp          Mean temperature (ºC) during the rearing period
#' @param duration      Duration (days) of rearing period
#' @param params        Parameters for the Ratkowsky growth model
#'
#' @export
#' @examples
#' floodplain_growth(10, 2, 20)
#' floodplain_growth(10, 20, 20)
#'

floodplain_growth <- function(mass, temp, duration, params = floodplain_growth_parameters){
  if(length(mass) != length(temp) || length(temp) != length(duration))
    stop("mass, temp, and duration must be the same length")

  p <- params
  omega <- p[["d"]] * (temp - p[["TL"]]) * (1 - exp(p[["g"]] * (temp - p[["TU"]])))
  (mass^p[["b"]] + (omega * p[["b"]] * duration)/100)^(1/p[["b"]])
}
