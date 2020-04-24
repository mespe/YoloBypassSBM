#' Length-weight relationshiop
#'
#' Return wet weight (g) for a given fork length (mm)
#'
#' @md
#' @param fork_length   Fork length (mm)
#' @param params        Parameters for length-weight relationship
#'
#' @export
#' @examples
#' length_weight(30:120)
#'

weight_length <- function(wet_weight, params = length_weight_parameters){
  (wet_weight/params[["a"]])^(1/params[["b"]])
}

