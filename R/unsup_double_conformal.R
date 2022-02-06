#' Unsupervised double conformal method
#'
#' @description
#' Take the following steps to construct the double conformal prediction interval:
#' 1. Apply order statistic method to each of the k groups to get k intervals
#' \[l_1, u_1\], ..., \[l_k, u_k\] each at level 1-alpha/2.
#' l_1 is the floor((n + 1) * alpha/4) order statistic of the first sample,
#' u_1 is the ceiling((n + 1)*(1 - alpha/4)) order statistic of the first sample,
#' and so on for the other samples.
#' 2. Consider (l_1, ..., l_k). Take l_(r), where r = floor((k + 1) * alpha/4).
#' Consider (u_1, ..., u_k). Take u_(s), where s = ceiling((k + 1)*(1 - alpha/4)).
#' The final prediction interval is C = \[l_(r), u_(s)\].
#'
#' @param Y List containing data of all subjects. Each item in the list
#' is a vector with one subject's observations.
#' @param alpha Significance level
#' @param n_val Number of observations from each subject. For this method,
#' we assume equal number of observations across subjects.
#' @param new_Y Observation on new subject
#'
#' @return List containing prediction interval size, prediction interval
#' lower bound, prediction interval upper bound, and whether new
#' observation is contained inside prediction interval.
#'
#' @export
unsup_double_conformal <- function(Y, alpha, n_val, new_Y = NULL) {

  # Construct 1-alpha/2 prediction intervals for each group/column.
  # There are n_val observations per group.
  group_pred_ints <-
    lapply(Y,
           FUN = function(x) unsup_pooled_pred_int(Y = x, alpha = alpha/2,
                                                   n_obs = n_val))

  # Get all lower bounds
  lower_bounds <- sapply(group_pred_ints, FUN = function(x) x$lower_bound)

  # Get all upper bounds
  upper_bounds <- sapply(group_pred_ints, FUN = function(x) x$upper_bound)

  # Get 1-alpha/2 prediction interval for lower bound.
  # There are k_val lower bound observations.
  lower_bound_pred_int <- unsup_pooled_pred_int(Y = lower_bounds,
                                                alpha = alpha/2, n_obs = k_val)

  # Get 1-alpha/2 prediction interval for upper bound.
  # There are k_val upper bound observations.
  upper_bound_pred_int <- unsup_pooled_pred_int(Y = upper_bounds,
                                                alpha = alpha/2, n_obs = k_val)

  # Prediction interval for new observation, using
  # lower bound of lower_bound_pred_int and
  # upper bound of upper_bound_pred_int
  pred_int <- list(lower_bound = lower_bound_pred_int$lower_bound,
                   upper_bound = upper_bound_pred_int$upper_bound)

  # Size of prediction interval
  pred_int_size <- pred_int$upper_bound - pred_int$lower_bound

  # Check whether new observation is inside interval
  if(!is.null(new_Y)) {

    covered <- as.numeric(pred_int$lower_bound <= new_Y &
                            new_Y <= pred_int$upper_bound)

  } else {

    covered <- NA

  }

  # Return prediction interval, interval size, and whether new Y is covered
  return(list(pred_int_size = pred_int_size,
              lower_bound = pred_int$lower_bound,
              upper_bound = pred_int$upper_bound,
              covered = covered))

  return(pred_int)
}

