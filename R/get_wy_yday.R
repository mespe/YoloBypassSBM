#' Day of water year
#'
#' Get day of water year from model day.
#'
#' @md
#' @param model_day Model day where 1 is 1996-10-02, 365 is 1997-10-01, 730 is 1998-10-01, etc.
#'
#' @export
#' @examples
#' get_wy_yday(c(1, 365, 730))
#'

get_wy_yday <- function(model_day){
  wy_yday <- function(x){
    x_lt <- as.POSIXlt(x)
    x_lt$yday + ifelse(x_lt$mon + 1L < 10L, 93,
                       ifelse(is_leap_year(x_lt$year), -273, -272))
  }
  # Date vector is same for all runs; choice of Fall is arbitrary
  wy_yday(knights_landing_fl_params[["Fall"]][["Date"]][model_day])
}

