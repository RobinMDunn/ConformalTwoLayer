#' Function to get p-val
#'
#' @param X_sample
#' @param Y_sample
#' @param X_new
#' @param Y_new
#'
#' @return
#' @export
#'
#' @examples
sup_get_pval <- function(X_sample, Y_sample, X_new, Y_new) {

  # Combine sample with new observation
  X_aug <- c(X_new, X_sample)
  Y_aug <- c(Y_new, Y_sample)

  # Fit model with augmented sample
  lm.fit <- lm(Y_aug ~ X_aug - 1)
  pred.fit <- lm.fit$fitted
  R <- abs(Y_aug - pred.fit)

  # Get p-value
  pval <- sum(R >= R[1]) / length(Y_aug)

  return(pval)
}
