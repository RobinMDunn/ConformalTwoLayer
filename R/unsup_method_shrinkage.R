# Function: shrinkage
# Implements shrinkage method 2.
#
# alpha: alpha level of conformal interval C
# mu: mean of normal dist used to generate theta_1, ..., theta_k
# tau: sd of normal dist used to generate theta_1, ..., theta_k
# k: number of groups
# n_j: number of Y observations in each group. Vector of length k
# theta: if available, set theta_j j=1,...,k to this value in all sims (Y_{ij} ~ N(theta_j,sqrt(sigma_sq)))
# sigma_sq: if available, set sigma_sq to this value in all sims (Y_{ij} ~ N(theta_j,sqrt(sigma_sq)))

#' Title
#'
#' @param Y
#' @param alpha
#' @param Y_new
#'
#' @return
#' @export
#'
#' @examples
unsup_method_shrinkage <- function(Y, alpha, Y_new = NULL){

  # Get p-values over a grid of Y_new values
  grid_values <- qnorm(p = seq(.01, .99, by = 0.01),
                       mean = mean(Y[[1]]), sd = sd(Y[[1]]))

  grid_pval_vec <- rep(NA, length(grid_values))

  for(val in 1:length(grid_values)) {
    grid_pval_vec[val] <- unsup_get_pval_shrinkage(Y_new = grid_values[val], Y = Y)
  }

  # Use root solver to get bounds of conformal interval
  # Approx. minimum Y_new where pval >= alpha.
  lower_bound <-
    uniroot(f = function(x) unsup_get_pval_shrinkage(Y_new = x, Y = Y) - (alpha - 0.0001),
            lower = min(grid_values[grid_pval_vec > alpha]) - 1,
            upper = min(grid_values[grid_pval_vec > alpha]),
            extendInt = "upX")$root

  # Approx. maximum Y_new where pval >= alpha.
  upper_bound <-
    uniroot(f = function(x) unsup_get_pval_shrinkage(Y_new = x, Y = Y) - (alpha - 0.0001),
            lower = max(grid_values[grid_pval_vec > alpha]),
            upper = max(grid_values[grid_pval_vec > alpha]) + 1,
            extendInt = "downX")$root

  # Get interval length
  pred_int_size <- upper_bound - lower_bound

  # Check if Y_new is in interval.
  if(is.null(Y_new)) {

    covered <- NA

  } else {

    # Get p-value of Y_new
    pval <- unsup_get_pval_shrinkage(Y_new = Y_new, Y = Y)

    # Y_new is covered is pval >= alpha
    covered <- as.numeric(pval >= alpha)

  }

  return(list(pred_int_size = pred_int_size,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              covered = covered))
}
