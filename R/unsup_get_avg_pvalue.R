#' Function to get average p-value
#'
#' @param Y_subsample_mat
#' @param point
#'
#' @return
#' @export
#'
#' @examples
unsup_get_avg_pvalue <- function(Y_subsample_mat, point) {

  # Create empty p_val_vec to store resampled p-values at point
  p_val_vec <- rep(NA, nrow(Y_subsample_mat))

  # Get p-values through resampling
  for(resamp in 1:nrow(Y_subsample_mat)) {

    # Get p-values based on subsampled observations
    p_val_vec[resamp] <- unsup_get_pvalue(u = point,
                                          Y_sorted = Y_subsample_mat[resamp, ])

  }

  # Return average p-value
  return(mean(p_val_vec))

}
