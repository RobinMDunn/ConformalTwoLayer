#' Evaluate p-value of potential y value Y_new compared to sample Y_1
#'
#' @param Y_new
#' @param Y_1
#'
#' @return
#' @export
#'
#' @examples
unsup_get_pval_group1 <- function(Y_new, Y_1){

  Y_aug <- c(Y_new, Y_1)
  ybar <- mean(Y_aug)
  R <- abs(Y_aug - ybar)
  pval <- sum(R >= R[1]) / length(Y_aug)
  return(pval)

}
