#' Leap year
#'
#' Check if a year is a leap year
#'
#' @md
#' @param year  Calendar year
#'
#' @export
#' @examples
#' is_leap_year(2000:2004)
#'

is_leap_year <- function(year){
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

