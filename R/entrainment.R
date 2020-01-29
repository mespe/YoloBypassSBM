#' Number of fish entrained at Fremont Weir
#'
#' Draw random number of fish entrained at Fremont Weir based on scenario, model day, and abundance at Fremont Weir
#'
#' @md
#' @param scenario      Scenario: Exg, Alt01, Alt04b, Alt04, Alt05, Alt06
#' @param model_day     Day that cohort reaches Fremont Weir
#' @param abundance     Abundance of cohort at Fremont Weir
#'
#'
#' @export
#'

entrainment <- function(scenario = c("Exg", "Alt01", "Alt04b", "Alt04", "Alt05", "Alt06"), model_day, abundance){
  scenario <- match.arg(scenario)
  entrained <- rbinom(1,
                      size = round(abundance),
                      fremont_weir_proportion[[scenario]][["Proportion"]][model_day])
  c("Yolo" = entrained, "Sac" = abundance - entrained)
}
