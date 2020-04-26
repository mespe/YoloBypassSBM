#' Rearing growth
#'
#' Returns final wet_weight given initial wet_weight, model day, and duration
#'
#' @md
#' @param wet_weight    Wet weight (g) at start of rearing period
#' @param model_day     Model day when rearing was initiated
#' @param duration      Duration (days) of rearing period
#'
#' @export
#' @examples
#' rearing_growth(10, 20, 2)
#' rearing_growth(10, 20, 20)
#'

rearing_growth <- function(wet_weight, model_day, duration){
  if(length(wet_weight) != length(model_day) || length(model_day) != length(duration))
    stop("wet_weight, model_day, and duration must be the same length")

  # subtract 1 because model_day is the first day of the rearing
  temp <- mapply(function (md, dur) mean(floodplain_temperature[["Value"]][md:(md + dur - 1)]),
                 model_day, duration)

  growth(wet_weight, temp, duration)
}
