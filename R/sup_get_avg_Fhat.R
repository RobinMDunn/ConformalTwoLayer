#' Estimate average F.hat_j(t) for CDF pooling with sample splitting
#'
#' @description Given residuals R_ji = |Y_ji - mu.hat(X_ji)| and a threshold
#' t, compute F.hat_j(t) = (1/n_j) sum_{i=1}^{n_j} I(R_ji <= t).
#' Then return average F.hat_j(t) over the subjects in the residual fitting
#' group.
#'
#' @param threshold Value t at which we compute average of F.hat_j(t)
#' @param resid_fit List of fitted residuals for supervised CDF pooling method
#'
#' @return Average F.hat_j(t) over the subjects in the residual fitting
#' group.
#'
#' @export
sup_get_avg_Fhat <- function(threshold, resid_fit) {

  # Within-group fractions of resid <= threshold
  F_hat_j <- lapply(resid_fit,
                    FUN = function(resid) mean(resid <= threshold))

  # Average of within-group fractions
  F_hat <- mean(unlist(F_hat_j))

  return(F_hat)
}
