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

length_weight <- function(fork_length, params = length_weight_parameters){
  params[["a"]]*fork_length^params[["b"]]
}

