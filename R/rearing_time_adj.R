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

  temps_all <- floodplain_temperature[["Value"]]
  helper <- function(md, dur){
    md_end <- md + dur
    # truncate rearing time to end of time period where input data available
    if (md_end > length(temps_all)) md_end <- length(temps_all)
    exceed_indices <- which(temps_all[md:md_end] > rearing_time_parameters[["thresh"]])
    # mininum of exceed_indices is first day that temp was over threshold in the max potential rearing period
    ifelse(length(exceed_indices) > 0, min(exceed_indices), 0)
  }

  mapply(helper, model_day, rearing_time_max)
}

