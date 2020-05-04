#' Passage growth
#'
#' Returns final wet_weight given initial wet_weight, model day, and duration
#'
#' @md
#' @param wet_weight    Wet weight (g) at start of rearing period
#' @param model_day     Model day when rearing was initiated
#' @param duration      Duration (days) of rearing period
#'
#' @export
#'

passage_growth <- function(wet_weight, model_day, duration){
  if(length(wet_weight) != length(model_day) || length(model_day) != length(duration))
    stop("wet_weight, model_day, and duration must be the same length")

  helper <- function(md, dur){
    mean(freeport_temperature[["Value"]][md:md + dur])
  }

  temp <- mapply(helper, model_day, duration)

  growth(wet_weight, temp, duration)
}
