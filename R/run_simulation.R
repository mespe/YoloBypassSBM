#' Run simulation
#'
#' Run simulation based on simulation parameters
#'
#' @md
#'
#' @export
#'
#'

run_simulation <- function(){

  params <- simulation_parameters
  set.seed(params[["random_seed"]])

  process_list <- function(input_list, col_name){
    list("Sac" = dplyr::bind_rows(lapply(input_list, "[[", "Sac"), .id = col_name),
         "Yolo" = dplyr::bind_rows(lapply(input_list, "[[", "Yolo"), .id = col_name))
  }

  # keeping Sac and Yolo separate because different number of columns
  for (i in 1:params[["reps"]]){
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
    if (!dir.exists("results")) dir.create("results")

    # carriage return, \r, allows for rewriting on same line
    cat("\r", "Rep", i, "of", params[["reps"]])

    saveRDS(process_list(wy_list, "WaterYear"),
            file.path("results", paste0(params[["name"]], "-Rep", i, ".rds")))
  }
}


