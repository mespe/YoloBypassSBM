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

  # subtract 1 because model_day is the first day of the rearing
  helper <- function(md, dur){
    md_end <- md + dur - 1
    mean(freeport_temperature[["Value"]][md:md_end])
  }

  # duration less than one means that no growth occurred
  # by setting to 1; avoid error and helper while ensuring no growth
  duration <- ifelse(duration < 1, 1, duration)

  temp <- mapply(helper, model_day, duration)

  growth(wet_weight, temp, duration)
}
