#' Unsupervised single subsample method
#'
#' @description Construct subsample of one observation per subject.
#' The p-value at u is inf\{alpha : u not in \[Y_{(r)}, Y_{(s)}\]\},
#' where r = floor((k+1)(alpha/2)) and s = ceiling((k+1)(1-alpha/2)).
#' The p-value is 1 if u is in \[Y_(floor((k+1)/2)), Y_(ceiling((k+1)/2))\].
#' The single subsampling prediction interval is the set of all u with
#' p-values >= alpha.
#'
#' @param Y List containing data of all subjects. Each item in the list
#' is a vector with one subject's observations.
#' @param alpha Significance level
#' @param k_val Number of subjects
#' @param new_Y Observation on new subject
#'
#' @return List containing prediction interval size, prediction interval
#' lower bound, prediction interval upper bound, and whether new
#' observation is contained inside prediction interval.
#'
#' @export
unsup_single_subsample <- function(Y, alpha, k_val, new_Y = NULL) {

  # Sample one observation from each of the k groups
  Y_subsample <- sapply(Y, FUN = function(x) sample(x, 1))

  # Construct prediction interval from Y_subsample
  pred_int <- unsup_pooled_pred_int(Y = Y_subsample, alpha = alpha,
                                    n_obs = k_val)

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
}
