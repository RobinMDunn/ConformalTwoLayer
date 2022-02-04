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
unsup_single_subsample <- function(Y, alpha, k_val) {

  # Sample one observation from each of the k groups
  Y_subsample <- sapply(Y, FUN = function(x) sample(x, 1))

  # Construct prediction interval from Y_subsample
  pred_int <- unsup_pooled_pred_int(Y = Y_subsample, alpha = alpha,
                                    n_obs = k_val)

  # Return prediction interval
  return(pred_int)
}
