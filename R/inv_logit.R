#' Inverse logit
#'
#' Return inverse logit of values. `-Inf` or `Inf` return logits of 0 or 1, respectively.
#'
#' @md
#' @param x             Values on logit scale
#'
#' @export
#'

inv_logit = function (x) {
  p = 1/(1 + exp(-x))
  p[is.infinite(p)] = 1
  p
}

