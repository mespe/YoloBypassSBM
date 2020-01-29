#' Water year
#'
#' Get water year from model day.
#'
#' @md
#' @param model_day Model day where 1 is 1996-10-02, 365 is 1997-10-01, 730 is 1998-10-01, etc.
#'
#' @export
#' @examples
#' get_water_year(c(1, 365, 730))
#'

get_water_year <- function(model_day){
  if (any(model_day < 1)) stop("model_day is not positive integer")
  if (any(model_day > 5382)) stop("model_day is outside range of data")

  water_year <- function(x){
    x_lt <- as.POSIXlt(x)
    x_lt$year + 1900L + ifelse(x_lt$mon + 1L >= 10L, 1L, 0L)
  }
  # Date vector is same for all runs; choice of Fall is arbitrary
  water_year(as.Date(knights_landing_fl_params[["Fall"]][["Date"]][model_day]))
}

