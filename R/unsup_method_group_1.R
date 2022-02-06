#' Use only subject 1's observations to construct prediction set for
#' a new subject 1 observation
#'
#' @description To check whether a potential y is in this interval, create
#' an augmented sample containing subject 1's observations and y.
#' As the nonconformity score for each observation in the augmented subject 1 data,
#' compute the absolute difference between each observation in the augmented sample
#' and the augmented mean. The p-value at y is the proportion of observations in the
#' augmented sample with nonconformity score >= y's nonconformity score.
#' The prediction set for a new observation on subject 1 is
#' \{y : p-value(y) >= alpha\}.
#'
#' @param Y List containing data of all subjects. Each item in the list
#' is a vector with one subject's observations. Only Y\[\[1\]\] is used.
#' @param alpha Significance level
#' @param Y_new New observation on subject 1
#'
#' @return List containing prediction interval size, prediction interval
#' lower bound, prediction interval upper bound, and whether new
#' observation is contained inside prediction interval.
#'
#' @export
unsup_method_group_1 <- function(Y, alpha, Y_new = NULL){

  # Get p-values over a grid of Y_new values
  grid_values <- qnorm(p = seq(.01, .99, by = 0.01),
                       mean = mean(Y[[1]]), sd = sd(Y[[1]]))

  grid_pval_vec <- rep(NA, length(grid_values))

  for(val in 1:length(grid_values)) {
    grid_pval_vec[val] <- unsup_get_pval_group1(u = grid_values[val],
                                                Y_1 = Y[[1]])
  }

  # Use root solver to get bounds of conformal interval
  # Approx. minimum u where pval >= alpha.
  lower_bound <-
    uniroot(f = function(x) unsup_get_pval_group1(u = x, Y_1 = Y[[1]]) -
              (alpha - 0.0001),
            lower = min(grid_values[grid_pval_vec > alpha]) - 1,
            upper = min(grid_values[grid_pval_vec > alpha]),
            extendInt = "upX")$root

  # Approx. maximum u where pval >= alpha.
  upper_bound <-
    uniroot(f = function(x) unsup_get_pval_group1(u = x, Y_1 = Y[[1]]) -
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
    pval <- unsup_get_pval_group1(u = Y_new, Y_1 = Y[[1]])

    # Y_new is covered is pval >= alpha
    covered <- as.numeric(pval >= alpha)

  }

  # Return prediction interval, interval size, and whether new Y is covered
  return(list(pred_int_size = pred_int_size,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              covered = covered))

}
