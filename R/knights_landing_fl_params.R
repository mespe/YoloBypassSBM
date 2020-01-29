#' Knights Landing fork length parameters
#'
#' A dataset containing the parameters used to draw random fork lengths for cohorts entering the model
#' in the Sacramento River at Knights Landing on each day of the 15-yr model period.
#'
#' @format A list comprised of 4 lists (one for each run: Fall, LateFall, Spring, Winter) that contain 3 vectors of 5382 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{MeanLog}{meanlog parameter of lognormal distribution}
#'   \item{SDLog}{sdlog parameter of lognormal distribution}
#' }
#'
"knights_landing_fl_params"
