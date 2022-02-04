#' Title
#'
#' @param Y
#' @param alpha
#' @param k_val
#'
#' @return
#' @export
#'
#' @examples
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
