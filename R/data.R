#' Annual abundance of outmigrants
#'
#' A dataset containing the annual outmigrant abundance that enters the model
#' in the Sacramento River at Knights Landing for each juvenile Chinook Salmon run
#' in each year of the 15-yr model period.
#'
#' @format A data frame with 15 rows and 5 columns:
#' \describe{
#'   \item{WaterYear}{defined as Oct 1st to Sept 30th}
#'   \item{Winter}{winter-run annual abundance}
#'   \item{Spring}{spring-run annual abundance}
#'   \item{Fall}{fall-run annual abundance}
#'   \item{LateFall}{late-fall-run annual abundance}
#' }
#'
"annual_abundance"

#' Freeport temperature
#'
#' A dataset containing the daily temperature in the Sacramento River at Freeport
#'
#' @format A list of 2 vectors of 5477 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{Value}{mean daily temperature (ºC)}
#' }
#'
"freeport_temperature"

#' Floodplain temperature
#'
#' A dataset containing the daily temperature on the floodplain
#'
#' @format A list of 2 vectors of 5477 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{Value}{mean daily temperature (ºC)}
#' }
#'
"floodplain_temperature"

#' Toe Drain temperature
#'
#' A dataset containing the daily temperature in the Toe Drain at the screw trap
#'
#' @format A list of 3 vectors of 5477 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{DOY}{day of year}
#'   \item{Value}{mean daily temperature (ºC)}
#' }
#'
"toe_drain_temperature"

#' Floodplain temperature difference
#'
#' A dataset containing the difference between in temperature between the Toe Drain and floodplain
#'
#' @format A list of 2 vectors of 366 elements:
#' \describe{
#'   \item{DOY}{day of year}
#'   \item{Value}{difference in mean daily temperature (ºC)}
#' }
#'
"floodplain_temperature_difference"

#' Proportion of fish entrained onto the Yolo Bypass at Fremont Weir
#'
#' A dataset containing the predicted proportion of fish entering the Yolo Bypass
#' for each day of the 15-yr model period.
#'
#' @format A list comprised of 2 vectors of 5477 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{Value}{proportion of fish entering the Yolo Bypass}
#' }
#'
"fremont_weir_proportion"

#' Flood duration
#'
#' Number of days where Yolo Bypass flows exceeded 4,000 cfs in a window of 120 days
#' from the date that cohort entered the Yolo Bypass.
#'
#' @format A list comprised of 2 vectors of 5477 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{Value}{flood duration (days)}
#' }
#'
"flood_duration"

#' Freeport Flow
#'
#' A dataset containing the modeled flow at Freeport.
#'
#' @format A list comprised of 2 vectors of 5477 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{Value}{flow (cfs) at Freeport}
#' }
#'
"freeport_flow"

#' Knights Landing fork length parameters
#'
#' A dataset containing the parameters used to draw random fork lengths for cohorts entering the model
#' in the Sacramento River at Knights Landing on each day of the 15-yr model period.
#'
#' @format A list comprised of 4 lists (one for each run: Fall, LateFall, Spring, Winter) that contain 3 vectors (Date, MeanLog, SDLog) of 5382 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{MeanLog}{meanlog parameter of lognormal distribution}
#'   \item{SDLog}{sdlog parameter of lognormal distribution}
#' }
#'
"knights_landing_fl_params"

#' Knights Landing entry timing
#'
#' A dataset containing the proportion of the population that enters the model
#' in the Sacramento River at Knights Landing on each day of the 15-yr model period.
#'
#' @format A data frame with 21528 rows and 7 columns:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{WaterYear}{water year associated with Date; defined as Oct 1st to Sept 30th}
#'   \item{Winter}{winter-run daily entry proportion}
#'   \item{Spring}{spring-run daily entry proportion}
#'   \item{Fall}{fall-run daily entry proportion}
#'   \item{LateFall}{late-fall-run daily entry proportion}
#'   \item{ModelDay}{model day where 1 is 1996-10-02, 365 is 1997-10-01, 730 is 1998-10-01, etc.}
#' }
#'
"knights_landing_timing"
