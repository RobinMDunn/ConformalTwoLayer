#' Evaluate average empirical CDF at a certain threshold
#'
#' @description For each subject j, compute the empirical CDF
#' F.hat_j(t) = (1/n_j) sum_{i=1}^{n_j} I(Y_ji <= t). Return the average
#' F.hat_j(t) over all subjects.
#'
#' @param Y List containing data of all subjects. Each item in the list
#' is a vector with one subject's observations.
#' @param threshold Value t at which we compute average of F.hat_j(t)
#'
#' @return Average F.hat_j(t) over all subjects
#'
#' @export
unsup_avg_ecdf <- function(Y, threshold) {

  ecdf_all <- sapply(Y, FUN = function(x) mean(x <= threshold))

  return(mean(ecdf_all))
}
