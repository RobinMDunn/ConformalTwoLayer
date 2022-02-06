#' Get shrinkage estimate of mean for subject 1
#'
#' @description Compute James-Stein shrinkage estimate of mean for subject 1,
#' using data from all subjects. For formula, see James-Stein Shrinkage
#' section of "Distribution-Free Prediction Sets with Two-Layer
#' Hierarchical Models" (Dunn et al., 2022).
#'
#' @param Y List containing data of all subjects. Each item in the list
#' is a vector with one subject's observations.
#'
#' @return James-Stein shrinkage estimator of subject 1 mean
#'
#' @export
unsup_get_Y_tilde_1 <- function(Y){

  # Number of distinct groups
  k <- length(Y)

  # Get means of k groups
  group_means <- sapply(Y, mean)

  # Get mean of group_means
  nu <- mean(group_means)

  # Get sample variance of augmented group 1
  sample_var_1 <- var(Y[[1]])

  # Get length of augmented group 1
  n_1 <- length(Y[[1]])

  # Get multiplicative factor for shrinkage
  mult_factor <- 1 - (k - 2) * (sample_var_1 / n_1) / sum((group_means - nu)^2)
  mult_factor <- max(0, mult_factor)

  # Compute Y_tilde_1
  Y_tilde_1 <- mult_factor * (group_means[1] - nu) + nu

  return(Y_tilde_1)
}
