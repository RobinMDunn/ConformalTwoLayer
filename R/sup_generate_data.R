#' Generate data for supervised normal examples
#'
#' @description Draw distribution-specific parameters
#' theta_1, ..., theta_k ~ N(mu, tau^2). For each distribution j,
#' draw covariates X_j1, ..., X_jn_j ~ N(0, 1), and draw outcomes
#' Y_{ji} = theta_j*X_{ji} + epsilon_{ji}, epsilon_{ji} ~ N(0, sigma^2).
#'
#' @param k Number of subjects
#' @param n Number of observations from each subject. For our examples,
#' we assume this is equal across subjects.
#' @param mu Mean of parameter distribution
#' @param tau_sq Variance of parameter distribution
#' @param sigma_sq Variance of epsilon in response model
#'
#' @return A list of length k, where each item contains a data frame of
#' observations from a single distribution
#'
#' @export
sup_generate_data <- function(k, n, mu, tau_sq, sigma_sq) {

  # Draw theta parameter for each of the k groups
  theta <- rnorm(n = k, mean = mu, sd = sqrt(tau_sq))

  # Generate X and Y data
  xy_data <- data.frame(matrix(ncol = 3, nrow = 0))

  for(j in 1:k){
    # Generate X_{j1}, ..., X_{jn_j} for j = 1, ..., k
    X_data <- rnorm(n = n, mean = 0, sd = 1)

    # Generate Y_{j1}, ..., Y_{jn_j} for j = 1, ..., k from
    # Y_j = theta_j * X_j + epsilon_j, where epsilon_j ~ N(0, sigma^2)
    Y_data <- theta[j] * X_data + rnorm(n = n, mean = 0, sd = sqrt(sigma_sq))

    # Combine X and Y data
    xy_data <- rbind(xy_data, data.frame(Subject = j, X1 = X_data, Y = Y_data))
  }

  return(xy_data)
}
