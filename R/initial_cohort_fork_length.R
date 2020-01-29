#' Draw initial cohort fork length
#'
#' Draw initial cohort for fork length based on run timing and model day
#'
#' @md
#' @param run           Run timing classification: Fall, LateFall, Winter, Spring
#' @param model_day     Day that cohort enters the model
#'
#'
#' @export
#'

initial_cohort_fork_length <- function(run = c("Fall", "LateFall", "Winter", "Spring"), model_day){
  run <- match.arg(run)
  rlnorm(1,
         knights_landing_fl_params[[run]][["MeanLog"]][model_day],
         knights_landing_fl_params[[run]][["SDLog"]][model_day])
}
