#' Title
#'
#' @param k
#' @param n
#' @param tau_sq
#'
#' @return
#' @export
unsup_generate_data <- function(k, n, tau_sq, sigma_sq = 1) {

  # Draw theta parameter for each of the k groups
  theta <- rnorm(n = k, mean = 0, sd = sqrt(tau_sq))

  # Generate n x k matrix (Y) with n observations for each of the k groups
  Y <- matrix(nrow = n, ncol = k)

  for(i in 1:k) {
    Y[, i] <- theta[i] + rnorm(n = n, mean = 0, sd = sqrt(sigma_sq))
  }

  return(Y)
}
