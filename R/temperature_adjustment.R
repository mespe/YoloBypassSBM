#' Adjust rearing time based on temperature
#'
#' Rearing time after potentially truncating based on temperature threshold
#'
#' @md
#' @param model_day     Model day when rearing was initiated
#' @param duration      Maximum numbr of rearing days
#' @param location      Rearing location: Yolo or Delta
#'
#' @export
#'
#'
if(FALSE){
temperature_adjustment <- function(model_day, duration, location){
  # this function is not vectorized

  # Temperature adjustment
  thresh <- rearing_time_parameters[[location]][["thresh"]]
  temps_all <- floodplain_temperature[[location]][["Value"]]

  if (duration == 0) return(0)
  model_day_end <- model_day + duration

  # truncate rearing time to end of time period where input data available
  if (model_day_end > length(temps_all)) model_day_end <- length(temps_all)
  temps_sub <- temps_all[model_day:model_day_end]

  # temps above threshold on every day
  if (all(temps_sub > thresh)) return(0)

  # mininum of exceed_indices is first day that temp was over threshold in the max potential rearing period
  exceed_indices <- which(temps_sub > thresh)
  return(ifelse(length(exceed_indices) > 0, min(exceed_indices), length(temps_all[model_day:model_day_end])))
}
}
temperature_adjustment <- function(model_day, duration, location){
    # this function is not vectorized
    if (duration == 0) return(0)
    
    # Temperature adjustment
    thresh <- rearing_time_parameters[[location]][["thresh"]]
    temps_all <- floodplain_temperature[[location]][["Value"]]
    
    model_day_end <- min(model_day + duration, length(temps_all))
    
  # truncate rearing time to end of time period where input data available
    temps_sub <- temps_all[model_day:model_day_end]
    
    # temps above threshold on every day
    if (min(temps_sub) > thresh) return(0)

    # mininum of exceed_indices is first day that temp was over threshold in the max potential rearing period
    exceed_indices <- which(temps_sub > thresh)
    return(if(length(exceed_indices)) exceed_indices[1] else model_day_end - model_day)
}

