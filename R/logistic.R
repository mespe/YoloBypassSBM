#' logistic function
#'
#' Four parameter logistic function
#'
#' @md
#' @param x   Independent variable
#' @param a   Maximum value
#' @param b   Steepness of curve
#' @param c   Inflection point
#' @param d   Minimum value
#'
#' @export
#'

logistic <- function(x, a, b, c, d){
  d + (a - d)/(1 + (x / c)^b)
}
