# Evaluate p-value of potential y value Y_new compared to sample Y_sample
#' Title
#'
#' @param Y_new
#' @param Y
#'
#' @return
#' @export
#'
#' @examples
unsup_get_pval_shrinkage <- function(Y_new, Y){

  # Augment data of group 1 with Y_new
  Y_aug <- c(Y_new, Y[[1]])
  Y[[1]] <- Y_aug

  # Compute Y_tilde_1
  Y_tilde_1 <- unsup_get_Y_tilde_1(Y)

  # Get p-value of Y_new
  R <- abs(Y_aug - Y_tilde_1)
  pval <- sum(R >= R[1]) / length(Y_aug)
  return(pval)

}
