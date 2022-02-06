#' Generate data for unsupervised normal examples
#'
#' @description Draw distribution-specific parameters
#' theta_1, ..., theta_k ~ N(mu, tau^2). For each distribution j,
#' draw observations Y_j1, ..., Y_jn_j ~ N(theta_j, sigma^2).
#'
#' @param k Number of subjects
#' @param n_vec Vector of length k, representing number of observations from
#' each subject. We allow this to vary across subjects.
#' @param mu Mean of parameter distribution
#' @param tau_sq Variance of parameter distribution
#' @param sigma_sq Variance of observations in given distribution
#'
#' @return A list of length k, where each item contains a vector of
#' observations from a single distribution
#'
#' @export
unsup_generate_data <- function(k, n_vec, mu, tau_sq, sigma_sq = 1) {

  # Draw theta parameter for each of the k groups
  theta <- rnorm(n = k, mean = mu, sd = sqrt(tau_sq))

  # Generate Y_{j1}, ..., Y_{jn_j} for j = 1, ..., k
  Y <- vector("list", k)
  for(j in 1:k){
    Y[[j]] <- rnorm(n = n_vec[j], mean = theta[j], sd = sqrt(sigma_sq))
  }

  return(Y)
}
