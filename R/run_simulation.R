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


run_simulation <- function(cores = parallel::detectCores()){

  params <- simulation_parameters
  set.seed(params[["random_seed"]])
  cat(params[["name"]], "\n")

  process_list <- function(input_list, col_name){
    list("Sac" = dplyr::bind_rows(lapply(input_list, "[[", "Sac"), .id = col_name),
         "Yolo" = dplyr::bind_rows(lapply(input_list, "[[", "Yolo"), .id = col_name))
  }

    iters = expand.grid(params[["water_years"]],
                        params[["chinook_runs"]], stringsAsFactors = FALSE)
  # keeping Sac and Yolo separate because different number of columns
    rep_list = parallel::mclapply(params[["reps"]], function(i) {

        ## This part is done in a loop
        run_list = lapply(seq(nrow(iters)), function(i)
          run_one_rep(water_year = iters[i,1],
                      chinook_run = iters[i,2],
                      sim_type = params[["sim_type"]]))
        names(run_list) = iters[,2]
        ## Summerize each water year
        wy_list = sapply(params[["water_years"]], function(wy){
            process_list(run_list[iters[,1] == wy], "Run")
        }, simplify = FALSE, USE.NAMES = TRUE)
                
        process_list(wy_list, "WaterYear")
    # carriage return, \r, allows for rewriting on same line
        #cat("\r", "Rep", i, "of", max(params[["reps"]]))
    }, mc.cores = cores)
  cat("\n")  # expectation is that cursor will generally be placed on next line, but this is insurance
  if (!dir.exists("results")) dir.create("results")

  saveRDS(process_list(rep_list, "Rep"),
          file.path("results", paste0(params[["name"]],
                                      "-Reps-",
                                      min(params[["reps"]]),
                                      "-",
                                      max(params[["reps"]]),
                                      ".rds")))
}


