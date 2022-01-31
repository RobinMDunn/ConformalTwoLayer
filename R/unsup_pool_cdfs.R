#' Title
#'
#' @param Y
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
unsup_pool_cdfs <- function(Y, alpha) {

  # Get average of ECDFs over grid
  grid_values <- quantile(Y, probs = seq(0, 1, by = 0.02))

  grid_pval_vec <- rep(NA, length(grid_values))

  for(val in 1:length(grid_values)) {
    grid_pval_vec[val] <- unsup_avg_ecdf(Y = Y, threshold = grid_values[val])
  }

  # Root solver to search for lower bound based on grid_pval_vec.
  # Approx. minimum threshold where avg_ecdf >= alpha/2.
  if(min(which(grid_pval_vec > alpha/2)) == 1) {
    lower_bound <-
      uniroot(f = function(x) unsup_avg_ecdf(Y = Y, threshold = x) - (alpha/2 - 0.0001),
              lower = min(Y) - 1,
              upper = min(Y),
              extendInt = "upX")$root
  } else {
    lower_bound <-
      uniroot(f = function(x) unsup_avg_ecdf(Y = Y, threshold = x) - (alpha/2 - 0.0001),
              lower = grid_values[min(which(grid_pval_vec > alpha/2)) - 1],
              upper = grid_values[min(which(grid_pval_vec > alpha/2))],
              extendInt = "upX")$root
  }

  # Root solver to search for upper bound based on grid_pval_vec.
  # Approx. minimum threshold where avg_ecdf >= 1 - alpha/2.
  if(min(which(grid_pval_vec > (1 - alpha/2))) == 1) {
    upper_bound <-
      uniroot(f = function(x) unsup_avg_ecdf(Y = Y, threshold = x) - (1 - alpha/2 - 0.0001),
              lower = min(Y) - 1,
              upper = min(Y),
              extendInt = "upX")$root
  } else {
    upper_bound <-
      uniroot(f = function(x) unsup_avg_ecdf(Y = Y, threshold = x) - (1 - alpha/2 - 0.0001),
              lower = grid_values[min(which(grid_pval_vec > 1 - alpha/2)) - 1],
              upper = grid_values[min(which(grid_pval_vec > 1 - alpha/2))],
              extendInt = "upX")$root
  }

  # Prediction interval for new observation
  pred_int <- list(lower_bound = lower_bound,
                   upper_bound = upper_bound)

  # Return prediction interval
  return(pred_int)
}
