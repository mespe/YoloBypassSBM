#' Day of year
#'
#' Get the model days in a water year
#'
#' @md
#' @param water_year Water year from the simulation period, 1997-2011
#'
#' @export
#' @examples
#' get_wy_model_days(1997)
#' get_wy_model_days(1998)
#' get_wy_model_days(2004)
#'

get_wy_model_days <- function(water_year){
  # quirk of history that model was chosen to start on Oct 2nd
  month_day <- ifelse(water_year == 1997, "-10-02", "-10-01")
  # num_days are all one less than expected because index starts with 0 in as.Date
  num_days <- ifelse(water_year == 1997, 363,
                     ifelse(is_leap_year(water_year), 365, 364))

  # Date vector is same for all runs; choice of Fall is arbitrary
  which(knights_landing_fl_params[["Fall"]][["Date"]] %in%
          as.Date(0:num_days, origin = paste0(water_year - 1, month_day)))
}
