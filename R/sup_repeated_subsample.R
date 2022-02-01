#' Title
#'
#' @param Y
#' @param alpha
#' @param n_val
#' @param k_val
#' @param n_resamp
#' @param X_new
#' @param Y_new
#'
#' @return
#' @export
sup_repeated_subsample <- function(xy_data, alpha, n_val, k_val, n_resamp,
                                   X_new, Y_new = NULL) {

  # Sample one (X,Y) from each of the k groups.
  index <- sample(1:n_val, size = k_val, replace = TRUE)
  X_sample <- sapply(as.list(1:k_val),
                     FUN = function(m) xy_data$X[[m]][index[m]])
  Y_sample <- sapply(as.list(1:k_val),
                     FUN = function(m) xy_data$Y[[m]][index[m]])

  # Create matrices to store repeated subsamples of (X, Y),
  # subsampling one observation from each of the k groups for each row.
  X_subsample_mat <- matrix(NA, nrow = n_resamp, ncol = k_val)
  Y_subsample_mat <- matrix(NA, nrow = n_resamp, ncol = k_val)

  for(resamp in 1:nrow(X_subsample_mat)) {

    # Sample one (X,Y) from each of the k groups.
    index <- sample(1:n_val, size = k_val, replace = TRUE)
    X_sample <- sapply(as.list(1:k_val),
                       FUN = function(m) xy_data$X[[m]][index[m]])
    Y_sample <- sapply(as.list(1:k_val),
                       FUN = function(m) xy_data$Y[[m]][index[m]])

    # Store observations in X_subsample_mat and Y_subsample_mat
    X_subsample_mat[resamp, ] <- X_sample
    Y_subsample_mat[resamp, ] <- Y_sample
  }

  # Get p-values over a grid of Y_new values
  grid_values <- seq(-10, 10, by = 1)

  grid_pval_vec <- rep(NA, length(grid_values))

  for(val in 1:length(grid_values)) {
    grid_pval_vec[val] <- sup_get_avg_pval(X_subsample_mat = X_subsample_mat,
                                           Y_subsample_mat = Y_subsample_mat,
                                           X_new = X_new,
                                           Y_new = grid_values[val])
  }

  # Use root solver to get bounds of conformal interval.
  # Approx. minimum Y_new where pval >= alpha.
  lower_bound <-
    uniroot(f = function(x) sup_get_avg_pval(X_subsample_mat = X_subsample_mat,
                                             Y_subsample_mat = Y_subsample_mat,
                                             X_new = X_new,
                                             Y_new = x) - (alpha - 0.0001),
            lower = min(grid_values[grid_pval_vec > alpha]) - 1,
            upper = min(grid_values[grid_pval_vec > alpha]),
            extendInt = "upX")$root

  # Approx. maximum Y_new where pval >= alpha.
  upper_bound <-
    uniroot(f = function(x) sup_get_avg_pval(X_subsample_mat = X_subsample_mat,
                                             Y_subsample_mat = Y_subsample_mat,
                                             X_new = X_new,
                                             Y_new = x) - (alpha - 0.0001),
            lower = max(grid_values[grid_pval_vec > alpha]),
            upper = max(grid_values[grid_pval_vec > alpha]) + 1,
            extendInt = "downX")$root

  # Store prediction interval size
  pred_int_size <- upper_bound - lower_bound

  # If we observe Y_new, check whether (X_new, Y_new) is inside interval
  if(is.null(Y_new)) {

    covered <- NA

  } else {

    # Check whether new observation is inside prediction sets
    covered <- as.numeric(
      sup_get_avg_pval(X_subsample_mat = X_subsample_mat,
                       Y_subsample_mat = Y_subsample_mat,
                       X_new = X_new, Y_new = Y_new) >= alpha)

  }

  # Return prediction interval, interval size, and whether new (X, Y) is covered
  return(list(pred_int_size = pred_int_size,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              covered = covered))

}

