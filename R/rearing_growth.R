#' Rearing growth
#'
#' Returns final wet_weight given initial wet_weight, model day, and duration
#'
#' @md
#' @param wet_weight    Wet weight (g) at start of rearing period
#' @param model_day     Model day when rearing was initiated
#' @param duration      Duration (days) of rearing period
#' @param location      Rearing location: Yolo or Delta
#'
#' @export
#'

rearing_growth <- function(wet_weight, model_day, duration, location){
  if(length(wet_weight) != length(model_day) || length(model_day) != length(duration))
    stop("wet_weight, model_day, and duration must be the same length")
  fpt = floodplain_temperature[[location]][["Value"]]
  temp <- mapply(helper, model_day, model_day + duration, MoreArgs = list(x = fpt))

  growth(wet_weight, temp, duration)
}

helper <- function(md, md2, x){
    mean(x[md:md2])
}

