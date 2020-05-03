#' Run simulation
#'
#' Run simulation based on simulation parameters. They typical workflow for running simulation is to reset_model_inputs(),
#' set (in the global environment) model input data and parameter for a simulation, and then use run_simulation().
#'
#' @md
#'
#' @export
#'
#'


run_simulation <- function(){

  params <- simulation_parameters

  process_list <- function(input_list, col_name){
    list("Sac" = dplyr::bind_rows(lapply(input_list, "[[", "Sac"), .id = col_name),
         "Yolo" = dplyr::bind_rows(lapply(input_list, "[[", "Yolo"), .id = col_name))
  }

  # keeping Sac and Yolo separate because different number of columns
  rep_list <- list()
  for (i in params[["reps"]]){
    wy_list <- list()
    for (j in params[["water_years"]]){
      run_list <- list()
      for (k in params[["chinook_runs"]]){
        scenario_list <- list()
        for (l in params[["scenarios"]]){
          scenario_list[[l]] <- run_one_rep(water_year = j,
                                            chinook_run = k,
                                            scenario = l,
                                            sim_type = params[["sim_type"]])
        }
        run_list[[k]] <- process_list(scenario_list, "Scenario")
      }
      wy_list[[as.character(j)]] <- process_list(run_list, "Run")
    }
    rep_list[[as.character(i)]] <- process_list(wy_list, "WaterYear")
    # carriage return, \r, allows for rewriting on same line
    cat("\r", "Rep", i, "of", max(params[["reps"]]))
  }
  if (!dir.exists("results")) dir.create("results")
  saveRDS(process_list(rep_list, "Rep"),
          file.path("results", paste0(params[["name"]],
                                      "-Reps-",
                                      min(params[["reps"]]),
                                      "-",
                                      max(params[["reps"]]),
                                      ".rds")))
}


