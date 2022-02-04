#' Title
#'
#' @param k
#' @param n
#' @param tau_sq
#' @param sigma_sq
#'
#' @return
#' @export
unsup_generate_data <- function(k, n_vec, tau_sq, sigma_sq = 1) {

  # Draw theta parameter for each of the k groups
  theta <- rnorm(n = k, mean = 0, sd = sqrt(tau_sq))

  # Generate Y_{j1}, ..., Y_{jn_j} for j = 1, ..., k
  Y <- vector("list", k)
  for(j in 1:k){
    Y[[j]] <- rnorm(n = n_vec[j], mean = theta[j], sd = sqrt(sigma_sq))
  }

  return(Y)
}
