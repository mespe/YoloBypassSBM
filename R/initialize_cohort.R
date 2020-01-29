#' Initialize cohort
#'
#' Initialize data structure for tracking cohort through model
#'
#' @md
#' @param run           Run of cohort
#' @param model_day     Day that cohort enters the model
#' @param abundance     Abundance at Knights Landing
#' @param fork_length   Fork length (mm) at Knights Landing
#'
#' @export
#'

initialize_cohort <- function(run = c("Fall", "LateFall", "Winter", "Spring"), model_day, abundance, fork_length){
  run <- match.arg(run)
  cohort <- cohort_data_template
  cohort[["Run"]] <- run
  cohort[["ModelDay"]][["Knights"]] <- model_day
  cohort[["Knights"]][["Abun"]] <- abundance
  cohort[["Knights"]][["FL"]] <- fork_length
  cohort
}


