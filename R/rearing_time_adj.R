#' Adjusted floodplain rearing time
#'
#' Floodplain rearing time adjusted for fish size and temperature
#'
#' @md
#' @param rearing_time_max    Maximum rearing duration (days)
#' @param wet_weight          Wet weight (g) at start of rearing period
#' @param model_day           Model day when rearing was initiated
#' @param temp_thresh         Threshold temperature when cohorts stop rearing
#'
#' @export
#' @examples
#' rearing_time_adj(20, 1, 100)
#' rearing_time_adj(20, 1, 200)
#' rearing_time_adj(20, 1, 210)
#' rearing_time_adj(20, 10, 100)
#' rearing_time_adj(20, 10, 210)
#'

rearing_time_adj <- function(rearing_time_max, wet_weight, model_day, temp_thresh = rearing_time_parameters[["thresh"]]){

  if(length(rearing_time_max) != length(wet_weight) || length(wet_weight) != length(model_day))
    stop("rearing_time_max, wet_weight, and model_day must be the same length")

  rt_size_adj <- round(rearing_time_max * rearing_time_proportion(weight_length(wet_weight)))

  helper <- function(md, dur){
    # subtract 1 because model_day is the first day of the rearing
    temps <- floodplain_temperature[["Value"]][md:(md + dur - 1)]
    length(temps[temps < temp_thresh])
  }
  mapply(helper, model_day, rt_size_adj)
}



