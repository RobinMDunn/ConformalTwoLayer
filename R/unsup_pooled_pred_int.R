#' Title
#'
#' @param Y
#' @param alpha
#' @param n_obs
#'
#' @return
#' @export
unsup_pooled_pred_int <- function(Y, alpha, n_obs) {

  # Convert entries of data matrix into a single vector
  Y <- as.vector(Y)

  # Sort observations from low to high
  Y <- sort(Y)

  # Get indices of lower and upper bounds
  r <- floor((n_obs + 1) * alpha/2)
  s <- ceiling((n_obs + 1) * (1 - alpha/2))

  # [Y_(r), Y_(s)] is the prediction interval for a new observation
  lower_bound <- Y[r]
  upper_bound <- Y[s]

  return(list(lower_bound = lower_bound,
              upper_bound = upper_bound))
}
