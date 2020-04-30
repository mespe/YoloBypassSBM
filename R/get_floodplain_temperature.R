#' Get floodplain temperature
#'
#' Returns floodplain temperature (sum of Toe Drain temperature and floodplain temperature difference);
#' used if you want to adjust floodplain_temperature_difference and recalculate floodplain temperature
#'
#' @md
#'
#'
#' @export
#'

get_floodplain_temperature <- function(){
  tdt <- toe_drain_temperature[["Value"]]
  # DOY is equivalent to index for floodplain_temperature_difference
  ftd <- floodplain_temperature_difference[["Value"]][toe_drain_temperature[["DOY"]]]
  list("Date" = toe_drain_temperature[["Date"]],
       "Value" = tdt + ftd)
}

