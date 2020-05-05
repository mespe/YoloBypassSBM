#' Floodplain rearing time
#'
#' Floodplain rearing time after adjusting indirectly for passage time and directly for temperature
#'
#' @md
#' @param model_day        Model day when rearing was initiated
#' @param scenario      Scenario: Exg, Alt01, Alt04b, Alt04, Alt05, Alt06
#' @param sim_type         Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

rearing_time <- function(model_day, scenario, sim_type){

  params = rearing_time_parameters
  fd <- flood_duration[[scenario]][["Value"]][model_day]
  rt_fd <- exp(params[["inter"]] + params[["slope"]] * fd)

  if (sim_type == "stochastic"){
    rt_fd <- sapply(rt_fd, function(rt) MASS::rnegbin(n = 1, mu = rt, theta = params[["theta"]]))
  }

  # subtracting a user-defined number of passage days
  # to account for fact that Takata relationship included
  # passage time as well as (presumably) rearing time
  rt_vals <- rt_fd - params[["passage_days"]]
  rt_vals <- ifelse(rt_vals < 0, 0, rt_vals)

  # Temperature adjustment
  thresh <- rearing_time_parameters[["thresh"]]
  temps_all <- floodplain_temperature[["Value"]]
  adjust_rt <- function(md, dur){
    if (dur == 0) return(0)
    md_end <- md + dur
    # truncate rearing time to end of time period where input data available
    if (md_end > length(temps_all)) md_end <- length(temps_all)
    temps_sub <- temps_all[md:md_end]
    if (all(temps_sub > thresh)) return(0)
    exceed_indices <- which(temps_sub > thresh)
    # mininum of exceed_indices is first day that temp was over threshold in the max potential rearing period
    return(ifelse(length(exceed_indices) > 0, min(exceed_indices), length(temps_all[md:md_end])))
  }

  mapply(adjust_rt, model_day, rt_vals)
}

