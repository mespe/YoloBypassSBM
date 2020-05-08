#' Day of year
#'
#' Get day of year from model day.
#'
#' @md
#' @param model_day Model day where 1 is 1996-10-02, 365 is 1997-10-01, 730 is 1998-10-01, etc.
#'
#' @export
#' @examples
#' get_yday(c(1, 365, 730))
#'

get_yday <- function(model_day){
  if (any(model_day < 1)) stop("model_day is not positive integer")
  # Date vector is same for all runs; choice of Fall is arbitrary
  as.POSIXlt(knights_landing_fl_params[["Fall"]][["Date"]][model_day])$yday + 1
}
