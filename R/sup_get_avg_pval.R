#' Function to get average p-val
#'
#' @param X_subsample_mat
#' @param Y_subsample_mat
#' @param X_new
#' @param Y_new
#'
#' @return
#' @export
#'
#' @examples
sup_get_avg_pval <- function(xy_subsample_list, model_formula, X_new, Y_new) {

  # Number of re-samples
  n_resamp <- length(xy_subsample_list)

  # Vector to store re-sampled p-values
  pval <- rep(NA, n_resamp)

  for(resamp in 1:n_resamp) {

    # Get p-value
    pval[resamp] <- sup_get_pval(xy_sample = xy_subsample_list[[resamp]],
                                 model_formula = model_formula,
                                 X_new = X_new,
                                 Y_new = Y_new)

  }

  return(mean(pval))
}
