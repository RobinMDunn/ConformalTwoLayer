# Function: get_Y_tilde_1
# Get Y_tilde_1 for group 1
#
# Y: List of observations, with one list element per group
#' Title
#'
#' @param Y
#'
#' @return
#' @export
#'
#' @examples
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
