#' Unsupervised repeated subsampling method
#'
#' @description Construct n_resamp subsamples of one observation per subject.
#' On a subsample b containing 1 observation from each of the k
#' subjects, the p-value at u is inf\{alpha : u not in \[Y^b_{(r)}, Y^b_{(s)}\]\},
#' where r = floor((k+1)(alpha/2)) and s = ceiling((k+1)(1-alpha/2)).
#' The p-value is 1 if u is in \[Y^b_(floor((k+1)/2)), Y^b_(ceiling((k+1)/2))\].
#' The repeated subsampling prediction interval is the set of all u with
#' average p-values >= alpha.
#'
#' @param Y List containing data of all subjects. Each item in the list
#' is a vector with one subject's observations.
#' @param alpha Significance level
#' @param k_val Number of subjects
#' @param n_resamp Number of repeated subsamples
#' @param new_Y Observation on new subject
#'
#' @return List containing prediction interval size, prediction interval
#' lower bound, prediction interval upper bound, and whether new
#' observation is contained inside prediction interval.
#'
#' @export
unsup_repeated_subsample <- function(Y, alpha, k_val, n_resamp,
                                     new_Y = NULL) {

  # Create matrix to store repeated subsamples of Y,
  # subsampling one observation from each of the k groups for each row.
  Y_subsample_mat <- matrix(NA, nrow = n_resamp, ncol = k_val)

  for(resamp in 1:nrow(Y_subsample_mat)) {
    # Sample one observation from each of the k groups
    Y_subsample <- sapply(Y, FUN = function(x) sample(x, 1))

    # Sort observations in subsample
    Y_sorted <- sort(Y_subsample)

    # Store sorted observations in Y_subsample_mat
    Y_subsample_mat[resamp, ] <- Y_sorted
  }

  # Get average p-value at new_Y
  if(!is.null(new_Y)) {
    new_Y_pval <- unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                       u = new_Y)

    # Check whether new observation is inside interval
    covered <- as.numeric(new_Y_pval >= alpha)

  } else {

    covered <- NA

  }

  # Get average p-values over grid
  grid_values <- quantile(unlist(Y), probs = seq(0, 1, by = 0.02))

  grid_pval_vec <- rep(NA, length(grid_values))

  for(val in 1:length(grid_values)) {
    grid_pval_vec[val] <- unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                               u = grid_values[val])
  }

  # Root solver to search for lower bound based on grid_pval_vec.
  # Approx. minimum value where avg_pvalue >= alpha.
  if(min(which(grid_pval_vec > alpha)) == 1) {
    lower_bound <-
      uniroot(f = function(x) unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                                   u = x) - (alpha - 0.0001),
              lower = min(unlist(Y)) - 1,
              upper = min(unlist(Y)),
              extendInt = "upX")$root
  } else {
    lower_bound <-
      uniroot(f = function(x) unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                                   u = x) - (alpha - 0.0001),
              lower = grid_values[min(which(grid_pval_vec > alpha)) - 1],
              upper = grid_values[min(which(grid_pval_vec > alpha))],
              extendInt = "upX")$root
  }

  # Root solver to search for upper bound based on grid_pval_vec.
  # Approx. maximum value where avg_pvalue >= alpha.
  if(max(which(grid_pval_vec > alpha)) == 51) {
    upper_bound <-
      uniroot(f = function(x) unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                                   u = x) - (alpha - 0.0001),
              lower = max(unlist(Y)),
              upper = max(unlist(Y)) + 1,
              extendInt = "downX")$root
  } else {
    upper_bound <-
      uniroot(f = function(x) unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                                   u = x) - (alpha - 0.0001),
              lower = grid_values[max(which(grid_pval_vec > alpha))],
              upper = grid_values[max(which(grid_pval_vec > alpha)) + 1],
              extendInt = "downX")$root
  }

  # Size of prediction interval
  pred_int_size <- upper_bound - lower_bound

  # Store prediction interval bounds and whether new_Y is covered (if applicable)
  results <- list(pred_int_size = pred_int_size,
                  lower_bound = lower_bound,
                  upper_bound = upper_bound,
                  covered = covered)

  # Return results
  return(results)

}
