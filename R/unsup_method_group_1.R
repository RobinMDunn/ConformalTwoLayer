#' Use only group 1's observations to construct prediction set for
#' a new group 1 observation
#'
#' @param Y
#' @param alpha
#' @param Y_new
#'
#' @return
#' @export
#'
#' @examples
unsup_method_group_1 <- function(Y, alpha, Y_new = NULL){

  # Get p-values over a grid of Y_new values
  grid_values <- qnorm(p = seq(.01, .99, by = 0.01),
                       mean = mean(Y[[1]]), sd = sd(Y[[1]]))

  grid_pval_vec <- rep(NA, length(grid_values))

  for(val in 1:length(grid_values)) {
    grid_pval_vec[val] <- unsup_get_pval_group1(Y_new = grid_values[val],
                                                Y_1 = Y[[1]])
  }

  # Use root solver to get bounds of conformal interval
  # Approx. minimum Y_new where pval >= alpha.
  lower_bound <-
    uniroot(f = function(x) unsup_get_pval_group1(Y_new = x, Y_1 = Y[[1]]) -
              (alpha - 0.0001),
            lower = min(grid_values[grid_pval_vec > alpha]) - 1,
            upper = min(grid_values[grid_pval_vec > alpha]),
            extendInt = "upX")$root

  # Approx. maximum Y_new where pval >= alpha.
  upper_bound <-
    uniroot(f = function(x) unsup_get_pval_group1(Y_new = x, Y_1 = Y[[1]]) -
              (alpha - 0.0001),
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
    pval <- unsup_get_pval_group1(Y_new = Y_new, Y_1 = Y[[1]])

    # Y_new is covered is pval >= alpha
    covered <- as.numeric(pval >= alpha)

  }

  # Return prediction interval, interval size, and whether new Y is covered
  return(list(pred_int_size = pred_int_size,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              covered = covered))

}
