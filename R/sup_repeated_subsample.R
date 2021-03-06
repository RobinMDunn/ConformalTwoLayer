#' Supervised repeated subsampling method
#'
#' @description Construct augmented subsamples containing one observation per
#' subject and (X_{k+1}, Y_{k+1}) = (X_new, Y_new). On each augmented subsample,
#' fit model mu.hat.
#' Compute nonconformity scores R_i = |mu.hat(X_i) - Y_i|, i = 1, ..., k+1.
#' The p-value pi_b for a given subsample is the proportion of R_i scores
#' greater than or equal to R_{k+1}. At a given X_new = x, the prediction set
#' is the set of all (x, y) with avg(pi_b) >= alpha.
#'
#' @param xy_data Data frame containing observations and outcomes for all
#' subjects. Must include a Subject column that identifies subjects.
#' @param model_formula Linear model formula for mu.hat which will be fit
#' on augmented samples of one observation per subject plus hypothetical data
#' on new subject
#' @param alpha Significance level
#' @param n_val Number of observations from each subject. For our examples,
#' we assume this is equal across subjects.
#' @param k_indices Labels of subjects to be treated as observed data
#' @param n_resamp Number of repeated subsamples
#' @param grid_values Vector of starting values to start for lower and upper
#' bounds of prediction interval. Should contain values across the range of Y.
#' @param new_xy_data Covariate and outcome data for new subject
#' @param coverage_only Indicates whether to only check coverage of new
#' observation, without constructing prediction interval.
#'
#' @return List containing prediction interval size at new observation's
#' covariate values, prediction interval
#' lower bound at new observation's covariate values, prediction interval
#' upper bound at new observation's covariate values, and whether new
#' observation's outcome is contained inside prediction interval.
#'
#' @export
sup_repeated_subsample <- function(xy_data, model_formula, alpha, n_val,
                                   k_indices, n_resamp, grid_values,
                                   new_xy_data, coverage_only = FALSE) {

  # Create list to store repeatedly subsampled data frames
  xy_subsample_list <- vector("list", n_resamp)

  # Extra new X data
  X_new <- dplyr::select(new_xy_data,
                         formula.tools::rhs.vars(model_formula))

  # Create matrices to store repeated subsamples of (X, Y),
  # subsampling one observation from each of the k groups for each row.
  for(resamp in 1:n_resamp) {

    # Sample one (X,Y) from each of the k groups.
    index_df <- data.frame(Subject = k_indices,
                           index = sample(1:n_val, size = length(k_indices),
                                          replace = TRUE))

    # Store subsampled data
    xy_subsample_list[[resamp]] <-
      do.call("rbind",
              lapply(k_indices,
                     FUN = function(i) xy_data[xy_data$Subject == i, ][index_df[index_df$Subject == i, ]$index, ]))

  }

  # Construct prediction set
  if(coverage_only == TRUE) {

    lower_bound <- upper_bound <- pred_int_size <- NA

  } else {

    # Get p-values over a grid of Y_new values
    grid_pval_vec <- rep(NA, length(grid_values))

    for(val in 1:length(grid_values)) {
      grid_pval_vec[val] <- sup_get_avg_pval(xy_subsample_list = xy_subsample_list,
                                             model_formula = model_formula,
                                             X_new = X_new,
                                             Y_new = grid_values[val])
    }

    # Use root solver to get bounds of conformal interval.
    # Approx. minimum Y_new where pval >= alpha.
    lower_bound <-
      uniroot(f = function(x) sup_get_avg_pval(xy_subsample_list = xy_subsample_list,
                                               model_formula = model_formula,
                                               X_new = X_new,
                                               Y_new = x) - (alpha - 0.0001),
              lower = min(grid_values[grid_pval_vec > alpha]) - 1,
              upper = min(grid_values[grid_pval_vec > alpha]),
              extendInt = "upX")$root

    # Approx. maximum Y_new where pval >= alpha.
    upper_bound <-
      uniroot(f = function(x) sup_get_avg_pval(xy_subsample_list = xy_subsample_list,
                                               model_formula = model_formula,
                                               X_new = X_new,
                                               Y_new = x) - (alpha - 0.0001),
              lower = max(grid_values[grid_pval_vec > alpha]),
              upper = max(grid_values[grid_pval_vec > alpha]) + 1,
              extendInt = "downX")$root

    # Store prediction interval size
    pred_int_size <- upper_bound - lower_bound

  }

  # If we observe Y_new, check whether (X_new, Y_new) is inside interval
  Y_new_observed <- as.character(formula.tools::lhs(model_formula)) %in%
    colnames(new_xy_data)

  # If we observe Y_new, check whether (X_new, Y_new) is inside interval
  if(Y_new_observed == FALSE) {

    covered <- NA

  } else {

    # Extract Y_new
    Y_new <- new_xy_data %>% dplyr::select(formula.tools::lhs(model_formula))

    # Check whether new observation is inside prediction sets
    covered <- as.numeric(
      sup_get_avg_pval(xy_subsample_list = xy_subsample_list,
                       model_formula = model_formula,
                       X_new = X_new, Y_new = Y_new) >= alpha)

  }

  # Return prediction interval, interval size, and whether new (X, Y) is covered
  return(list(pred_int_size = pred_int_size,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              covered = covered))

}

