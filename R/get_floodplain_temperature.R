#' Get floodplain temperature
#'
#' Returns floodplain temperature (sum of Toe Drain/Freeport temperature and floodplain temperature difference);
#' used to adjust floodplain_temperature_difference and recalculate floodplain temperature
#'
#' @md
#'
#'
#' @export
#'

get_floodplain_temperature <- function(){
  # DOY is equivalent to index for floodplain_temperature_difference
  ftd_yolo <- floodplain_temperature_difference[["Yolo"]][["Value"]][toe_drain_temperature[["DOY"]]]
  ftd_delta <- floodplain_temperature_difference[["Delta"]][["Value"]][freeport_temperature[["DOY"]]]

  list("Yolo" = list("Date" = toe_drain_temperature[["Date"]],
                     "Value" = toe_drain_temperature[["Value"]] + ftd_yolo),
       "Delta" = list("Date" = freeport_temperature[["Date"]],
                      "Value" = freeport_temperature[["Value"]] + ftd_delta))
}

