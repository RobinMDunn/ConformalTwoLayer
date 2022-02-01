# Estimate Fhat at given threshold
#'
#' @param threshold
#' @param resid_fit
#'
#' @return
#' @export
#'
#' @examples
sup_get_avg_Fhat <- function(threshold, resid_fit) {

  # Within-group fractions of resid <= threshold
  F_hat_j <- lapply(resid_fit,
                    FUN = function(resid) mean(resid <= threshold))

  # Average of within-group fractions
  F_hat <- mean(unlist(F_hat_j))

  return(F_hat)
}
