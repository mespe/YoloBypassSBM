#' Reset model inputs
#'
#' Reset model input data and parameters by removing any items with the same names from the global environment.
#'
#'
#' @md
#'
#' @export
#'
#'

reset_model_inputs <- function(){
  rm(list = c("annual_abundance",
              "cohort_data_template",
              "flood_duration",
              "floodplain_temperature_difference",
              "floodplain_temperature",
              "freeport_flow",
              "freeport_temperature",
              "fremont_weir_proportion",
              "growth_parameters",
              "knights_landing_fl_params",
              "knights_landing_timing",
              "length_weight_parameters",
              "ocean_survival_parameters",
              "rearing_proportion_parameters",
              "rearing_survival_parameters",
              "rearing_time_parameters",
              "simulation_parameters",
              "telemetry_parameters",
              "toe_drain_temperature"),
     envir = .GlobalEnv)
}

