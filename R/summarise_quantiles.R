#' Summarise quantiles of replicates in a grouped dataframe
#'
#' Common operation to summarise model results across replicates.
#'
#' @md
#' @param grouped_df     Grouped data frame (usually formed by dplyr::group_by)
#' @param column         Column to summarise
#'
#' @export
#'

summarise_quantiles = function (grouped_df, column) {
  require(dplyr)
  grouped_df %>%
    summarise(Lwr = quantile(.data[[column]], probs = 0.025),
              Median = median(.data[[column]]),
              Upr = quantile(.data[[column]], probs = 0.975))
}



