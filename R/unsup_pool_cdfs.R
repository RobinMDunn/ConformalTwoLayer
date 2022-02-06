#' Unsupervised CDF pooling method
#'
#' @description Construct CDF pooling prediction interval for a new subject.
#' At any value t and for each subject j, the empirical CDF is given by
#' F.hat_j(t) = (1/n_j) sum_{i=1}^{n_j} I(Y_ji <= t). We let
#' q.hat(alpha) = inf\{t : (1/k) sum_{j=1}^k F.hat_j(t) >= alpha\}.
#' The CDF pooling prediction interval is \[q.hat(alpha/2), q.hat(1-alpha/2)\].
#'
#' @param Y List containing data of all subjects. Each item in the list
#' is a vector with one subject's observations.
#' @param alpha Significance level
#' @param new_Y Observation on new subject
#'
#' @return List containing prediction interval size, prediction interval
#' lower bound, prediction interval upper bound, and whether new
#' observation is contained inside prediction interval.
#'
#' @export
unsup_pool_cdfs <- function(Y, alpha, new_Y = NULL) {

  # Get average of ECDFs over grid
  grid_values <- quantile(unlist(Y), probs = seq(0, 1, by = 0.02))

  grid_pval_vec <- rep(NA, length(grid_values))

  for(val in 1:length(grid_values)) {
    grid_pval_vec[val] <- unsup_avg_ecdf(Y = Y, threshold = grid_values[val])
  }

  # Root solver to search for lower bound based on grid_pval_vec.
  # Approx. minimum threshold where avg_ecdf >= alpha/2.
  if(min(which(grid_pval_vec > alpha/2)) == 1) {
    lower_bound <-
      uniroot(f = function(x) unsup_avg_ecdf(Y = Y, threshold = x) - (alpha/2 - 0.0001),
              lower = min(unlist(Y)) - 1,
              upper = min(unlist(Y)),
              extendInt = "upX")$root
  } else {
    lower_bound <-
      uniroot(f = function(x) unsup_avg_ecdf(Y = Y, threshold = x) - (alpha/2 - 0.0001),
              lower = grid_values[min(which(grid_pval_vec > alpha/2)) - 1],
              upper = grid_values[min(which(grid_pval_vec > alpha/2))],
              extendInt = "upX")$root
  }

  # Root solver to search for upper bound based on grid_pval_vec.
  # Approx. minimum threshold where avg_ecdf >= 1 - alpha/2.
  if(min(which(grid_pval_vec > (1 - alpha/2))) == 1) {
    upper_bound <-
      uniroot(f = function(x) unsup_avg_ecdf(Y = Y, threshold = x) - (1 - alpha/2 - 0.0001),
              lower = min(unlist(Y)) - 1,
              upper = min(unlist(Y)),
              extendInt = "upX")$root
  } else {
    upper_bound <-
      uniroot(f = function(x) unsup_avg_ecdf(Y = Y, threshold = x) - (1 - alpha/2 - 0.0001),
              lower = grid_values[min(which(grid_pval_vec > 1 - alpha/2)) - 1],
              upper = grid_values[min(which(grid_pval_vec > 1 - alpha/2))],
              extendInt = "upX")$root
  }

  # Size of prediction interval
  pred_int_size <- upper_bound - lower_bound

  # Check whether new observation is inside interval
  if(!is.null(new_Y)) {

    covered <-
      as.numeric(unsup_avg_ecdf(Y = Y, threshold = new_Y) >= alpha/2 &
                   unsup_avg_ecdf(Y = Y, threshold = new_Y) < 1 - alpha/2)

  } else {

    covered <- NA

  }

  # Return prediction interval, interval size, and whether new Y is covered
  return(list(pred_int_size = pred_int_size,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              covered = covered))

  return(pred_int)
}
