#' Construct intervals based on order statistics
#'
#' @description Helper function for unsupervised double conformal method.
#' Construct interval defined by order statistics of sample:
#'  \[Y_(floor((n_obs + 1) * alpha/2)), Y_(ceiling((n_obs + 1) * (1 - alpha/2)))\].
#'
#' @param Y Vector of observations
#' @param alpha Significance level
#' @param n_obs Number of observations in vector Y
#'
#' @return List containing lower bound and upper bound of interval
#'
#' @export
unsup_pooled_pred_int <- function(Y, alpha, n_obs) {

  # Sort observations from low to high
  Y <- sort(Y)

  # Get indices of lower and upper bounds
  r <- floor((n_obs + 1) * alpha/2)
  s <- ceiling((n_obs + 1) * (1 - alpha/2))

  # [Y_(r), Y_(s)] defines the interval
  lower_bound <- Y[r]
  upper_bound <- Y[s]

  return(list(lower_bound = lower_bound,
              upper_bound = upper_bound))
}
