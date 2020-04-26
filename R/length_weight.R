#' Length-weight relationshiop
#'
#' Return wet weight (g) for a given fork length (mm)
#'
#' @md
#' @param fork_length   Fork length (mm)
#'
#' @export
#' @examples
#' length_weight(30:120)
#'

length_weight <- function(fork_length){
  p <- length_weight_parameters
  p[["a"]]*fork_length^p[["b"]]
}

