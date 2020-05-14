#' Extract selected columns from raw model output
#'
#' Returns a list with two dataframes (Sac and Yolo).
#'
#' @md
#' @param sim_name       Name of simulation, e.g., 'Baseline' would be used to match files named as Baseline-Reps-1-50.rds, Baseline-Reps-51-100.rds, etc.
#' @param sac_columns    Vector of Sac column names to extract
#' @param column         Vector of Yolo column names to extract
#'
#' @export
#'

extract_results = function (sim_name, sac_columns, yolo_columns) {
  
  file_names <- list.files(path = "results", pattern = sim_name, full.names = TRUE)

  extracted_list <- lapply(file_names,
                           function (fn){
                             res_list <- readRDS(fn)
                             list("Sac" = res_list[["Sac"]][,sac_columns],
                                  "Yolo" = res_list[["Yolo"]][,yolo_columns])})

  list("Sac" = bind_rows(lapply(extracted_list, "[[", "Sac")),
       "Yolo" = bind_rows(lapply(extracted_list, "[[", "Yolo")))
}



