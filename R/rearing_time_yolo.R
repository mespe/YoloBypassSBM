#' Yolo Bypass rearing time
#'
#' Yolo Bypass rearing time after adjusting indirectly for passage time and directly for temperature
#'
#' @md
#' @param model_day     Model day when rearing was initiated
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

rearing_time_yolo <- function(model_day, sim_type){

  params = rearing_time_parameters[["Yolo"]]
  fd <- flood_duration[["Value"]][model_day]
  rt_fd <- exp(params[["inter"]] + params[["slope"]] * fd)

  if (sim_type == "stochastic"){
    rt_fd <- sapply(rt_fd, function(rt) MASS::rnegbin(n = 1, mu = rt, theta = params[["theta"]]))
  }

  # subtracting a user-defined number of passage days
  # to account for fact that Takata relationship included
  # passage time as well as (presumably) rearing time
  rt_vals <- rt_fd - params[["passage_days"]]
  rt_vals <- ifelse(rt_vals < 0, 0, rt_vals)

  mapply(function(md, dur) temperature_adjustment(md, dur, "Yolo"), model_day, rt_vals)
}

