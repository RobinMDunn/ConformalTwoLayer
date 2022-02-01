#' Function to get average p-val
#'
#' @param X_subsample_mat
#' @param Y_subsample_mat
#' @param X_new
#' @param Y_new
#'
#' @return
#' @export
#'
#' @examples
sup_get_avg_pval <- function(X_subsample_mat, Y_subsample_mat, X_new, Y_new) {

  # Number of re-samples
  n_resamp <- nrow(X_subsample_mat)

  # Vector to store re-sampled p-values
  pval <- rep(NA, n_resamp)

  for(resamp in 1:n_resamp) {

    # Combine sample with new observation
    X_aug <- c(X_new, X_subsample_mat[resamp, ])
    Y_aug <- c(Y_new, Y_subsample_mat[resamp, ])

    # Fit model with augmented sample
    lm.fit <- lm(Y_aug ~ X_aug - 1)
    pred.fit <- lm.fit$fitted
    R <- abs(Y_aug - pred.fit)

    # Get p-value
    pval[resamp] <- sum(R >= R[1]) / length(Y_aug)
  }

  return(mean(pval))
}
