#' Evaluate p-value of u compared to subject 1's sample
#'
#' @description Helper function for method that predicts a new observation on
#' subject 1 using only data from subject 1. Create augmented sample
#' containing subject 1's observations and u. As nonconformity scores, compute
#' the absolute difference between each observation in augmented sample and
#' the augmented mean. The p-value is the proportion of observations in the
#' augmented sample with nonconformity score >= u's nonconformity score.
#'
#' @param u Hypothetical new observation from subject 1
#' @param Y_1 Vector of observations from subject 1
#'
#' @return p-value of u relative to subject 1's sample
#'
#' @export
unsup_get_pval_group1 <- function(u, Y_1){

  # Compute mean of augmented sample containing u and Y_1
  Y_aug <- c(u, Y_1)
  ybar <- mean(Y_aug)

  # Nonconformity scores are absolute difference between each observation
  # in augmented sample and the augmented mean
  R <- abs(Y_aug - ybar)

  # p-value is proportion of observations in augmented sample with
  # nonconformity score >= u's nonconformity score
  pval <- sum(R >= R[1]) / length(Y_aug)
  return(pval)

}
