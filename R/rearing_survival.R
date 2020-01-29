#' Draw rearing survival
#'
#' Draw a random rearing survival based on rearing time and daily survival
#'
#' @md
#' @param rearing_time  Number of days spent rearing
#' @param min,max       Lower and upper limits of uniform distribution of daily survival values
#'
#' @export
#' @examples
#' rearing_survival(10, 0.95, 0.999)
#'

rearing_survival <- function(rearing_time, min, max){
  # simplest possible version for now
  # would like to incorporate seasonality and possible size/stage effects
  # this formulation converts daily survival to instantaneous survival
  exp(log(runif(1, min, max))*rearing_time)
}
