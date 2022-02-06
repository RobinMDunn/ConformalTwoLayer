#' Get average p-value for unsupervised repeated subsampling method
#'
#' @description On a subsample b containing 1 observation from each of the k
#' subjects, the p-value at u is inf\{alpha : u not in \[Y^b_{(r)}, Y^b_{(s)}\]\},
#' where r = floor((k+1)(alpha/2)) and s = ceiling((k+1)(1-alpha/2)).
#' The p-value is 1 if u is in \[Y^b_(floor((k+1)/2)), Y^b_(ceiling((k+1)/2))\].
#' Compute p-value of u on each subsample, and return the average p-value.
#'
#' @param Y_subsample_mat Data frame where each row is a subsample of one
#' observation from each of the k subjects
#' @param u Observation at which to compute p-value
#'
#' @return Average p-value of u across repeated subsamples
#'
#' @export
unsup_get_avg_pvalue <- function(Y_subsample_mat, u) {

  # Create empty p_val_vec to store resampled p-values at u
  p_val_vec <- rep(NA, nrow(Y_subsample_mat))

  # Get p-values through resampling
  for(resamp in 1:nrow(Y_subsample_mat)) {

    # Get p-values based on subsampled observations
    p_val_vec[resamp] <- unsup_get_pvalue(u = u,
                                          Y_sorted = Y_subsample_mat[resamp, ])

  }

  # Return average p-value
  return(mean(p_val_vec))

}
