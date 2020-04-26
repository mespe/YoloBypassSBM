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

weight_length <- function(wet_weight){
  p <- length_weight_parameters
  (wet_weight/p[["a"]])^(1/p[["b"]])
}

