#' Title
#'
#' @param k
#' @param n
#' @param mu
#' @param tau_sq
#' @param sigma_sq
#'
#' @return
#' @export
#'
#' @examples
sup_generate_data <- function(k, n, mu, tau_sq, sigma_sq) {

  # Draw theta parameter for each of the k groups
  theta <- rnorm(n = k, mean = mu, sd = sqrt(tau_sq))

  # Generate X_{j1}, ..., X_{jn_j} for j = 1, ..., k
  X <- vector("list", k)
  for(j in 1:k){
    X[[j]] <- rnorm(n = n, mean = 0, sd = 1)
  }

  # Generate Y_{j1}, ..., Y_{jn_j} for j = 1, ..., k from
  # Y_j = theta_j * X_j + epsilon_j, where epsilon_j ~ N(0, sigma^2)
  Y <- vector("list", k)
  for(j in 1:k){
    Y[[j]] <- theta[j]*X[[j]] + rnorm(n = n, mean = 0, sd = sqrt(sigma_sq))
  }

  return(list(X = X, Y = Y))
}
