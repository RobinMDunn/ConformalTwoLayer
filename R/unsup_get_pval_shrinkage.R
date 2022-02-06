#' Evaluate p-value of u using James-Stein shrinkage
#'
#' @description Create augmented sample containing subject 1's observations
#' and u. Compute James-Stein shrinkage estimate of subject 1 mean using
#' augmented subject 1 sample and data from all other subjects.
#' As nonconformity scores, compute the absolute difference between each
#' observation in the augmented subject 1 sample and the James-Stein shrinkage
#' estimate. The p-value is the proportion of observations in the augmented
#' sample with nonconformity score >= u's nonconformity score.
#'
#' @param u Hypothetical new observation on subject 1
#' @param Y List containing data of all subjects. Each item in the list
#' is a vector with one subject's observations.
#'
#' @return p-value of u relative to subject 1's sample, using the James-Stein
#' shrinkage estimate
#'
#' @export
unsup_get_pval_shrinkage <- function(u, Y){

  # Augment data of group 1 with u
  Y_aug <- c(u, Y[[1]])
  Y[[1]] <- Y_aug

  # Compute Y_tilde_1
  Y_tilde_1 <- unsup_get_Y_tilde_1(Y)

  # Get p-value of u
  R <- abs(Y_aug - Y_tilde_1)
  pval <- sum(R >= R[1]) / length(Y_aug)
  return(pval)

}
