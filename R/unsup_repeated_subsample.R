#' Title
#'
#' @param Y
#' @param alpha
#' @param k_val
#' @param new_Y (needs to be optional)
#'
#' @return
#' @export
#'
#' @examples
unsup_repeated_subsample <- function(Y, alpha, k_val, n_resamp, new_Y) {

  # Create matrix to store repeated subsamples of Y,
  # subsampling one observation from each of the k groups for each row.
  Y_subsample_mat <- matrix(NA, nrow = n_resamp, ncol = k_val)

  for(resamp in 1:nrow(Y_subsample_mat)) {
    # Sample one observation from each of the k groups
    Y_subsample <- apply(Y, MARGIN = 2, FUN = function(x) sample(x, 1))

    # Sort observations in subsample
    Y_sorted <- sort(Y_subsample)

    # Store sorted observations in Y_subsample_mat
    Y_subsample_mat[resamp, ] <- Y_sorted
  }

  # Get average p-value at new_Y
  new_Y_pval <- unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                     point = new_Y)

  # Check whether new observation is inside interval
  new_Y_covered <- as.numeric(new_Y_pval >= alpha)

  # Get average p-values over grid
  grid_values <- quantile(Y, probs = seq(0, 1, by = 0.02))

  grid_pval_vec <- rep(NA, length(grid_values))

  for(val in 1:length(grid_values)) {
    grid_pval_vec[val] <- unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                               point = grid_values[val])
  }

  # Root solver to search for lower bound based on grid_pval_vec.
  # Approx. minimum value where avg_pvalue >= alpha.
  if(min(which(grid_pval_vec > alpha)) == 1) {
    lower_bound_2alpha <-
      uniroot(f = function(x) unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                                   point = x) - (alpha - 0.0001),
              lower = min(Y) - 1,
              upper = min(Y),
              extendInt = "upX")$root
  } else {
    lower_bound_2alpha <-
      uniroot(f = function(x) unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                                   point = x) - (alpha - 0.0001),
              lower = grid_values[min(which(grid_pval_vec > alpha)) - 1],
              upper = grid_values[min(which(grid_pval_vec > alpha))],
              extendInt = "upX")$root
  }

  # Root solver to search for upper bound based on grid_pval_vec.
  # Approx. maximum value where avg_pvalue >= alpha.
  if(max(which(grid_pval_vec > alpha)) == 51) {
    upper_bound_2alpha <-
      uniroot(f = function(x) unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                                   point = x) - (alpha - 0.0001),
              lower = max(Y),
              upper = max(Y) + 1,
              extendInt = "downX")$root
  } else {
    upper_bound_2alpha <-
      uniroot(f = function(x) unsup_get_avg_pvalue(Y_subsample_mat = Y_subsample_mat,
                                                   point = x) - (alpha - 0.0001),
              lower = grid_values[max(which(grid_pval_vec > alpha))],
              upper = grid_values[max(which(grid_pval_vec > alpha)) + 1],
              extendInt = "downX")$root
  }

  # Store prediction interval bounds and whether new_Y is covered (if applicable)
  results <- list(lower_bound_2alpha = lower_bound_2alpha,
                  upper_bound_2alpha = upper_bound_2alpha,
                  new_Y_covered = new_Y_covered)

  # Return results
  return(results)

}
