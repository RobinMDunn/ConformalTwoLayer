#' Evaluate average empirical CDF at a certain threshold
#'
#' @param Y
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
unsup_avg_ecdf <- function(Y, threshold) {
  ecdf_all <- sapply(Y, FUN = function(x) mean(x <= threshold))
  return(mean(ecdf_all))
}
