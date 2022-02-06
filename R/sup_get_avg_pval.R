#' Get average p-value for supervised repeated subsampling method
#'
#' @description On each augmented subsample containing one observation per
#' subject and (X_{k+1}, Y_{k+1}) = (X_new, Y_new), fit model mu.hat.
#' Compute nonconformity scores R_i = |mu.hat(X_i) - Y_i|, i = 1, ..., k+1.
#' The p-value pi_b for a given subsample is the proportion of R_i scores
#' greater than or equal to R_{k+1}. Return average pi_b over all subsamples.
#'
#' @param xy_subsample_list List of data frames, where each data frame contains
#' one observation per subject
#' @param model_formula Linear model formula for mu.hat which will be fit
#' on augmented sample
#' @param X_new Covariate information for new observation
#' @param Y_new Hypothetical outcome for new observation
#'
#' @return Average p-value for new observation (X_new, Y_new)
#'
#' @export
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
