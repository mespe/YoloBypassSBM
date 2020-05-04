#' Adjusted floodplain rearing time
#'
#' Floodplain rearing time adjusted for fish size and temperature
#'
#' @md
#' @param rearing_time_max    Maximum rearing duration (days)
#' @param model_day           Model day when rearing was initiated
#'
#' @export
#'
#'

rearing_time_adj <- function(rearing_time_max, model_day){

  if(length(rearing_time_max) != length(model_day))
    stop("rearing_time_max and model_day must be the same length")

  helper <- function(md, dur){
    temps <- floodplain_temperature[["Value"]][md:(md + dur)]
    length(temps[temps < rearing_time_parameters[["thresh"]]])
  }

  mapply(helper, model_day, rearing_time_max)
}

