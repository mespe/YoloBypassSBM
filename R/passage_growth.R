#' Passage growth
#'
#' Returns final wet_weight given initial wet_weight, model day, and duration
#'
#' @md
#' @param wet_weight    Wet weight (g) at start of rearing period
#' @param model_day     Model day when rearing was initiated
#' @param duration      Duration (days) of rearing period
#' @param route         Route: Sacramento River (Sac) or Yolo Bypass (Yolo)
#'
#' @export
#'

passage_growth <- function(wet_weight, model_day, duration, route){
  if(length(wet_weight) != length(model_day) || length(model_day) != length(duration))
    stop("wet_weight, model_day, and duration must be the same length")

  helper <- function(md, dur){
    if (route == "Sac"){
      temps_all <- freeport_temperature[["Value"]]
    } else {
      temps_all <- toe_drain_temperature[["Value"]]
    }
    mean(temps_all[md:(md + dur)])
  }

  temp <- mapply(helper, model_day, duration)

  growth(wet_weight, temp, duration)
}
