#' Get p-value for supervised subsampling methods
#'
#' @description On a single augmented subsample containing one observation per
#' subject and (X_{k+1}, Y_{k+1}) = (X_new, Y_new), fit model mu.hat.
#' Compute nonconformity scores R_i = |mu.hat(X_i) - Y_i|, i = 1, ..., k+1.
#' The p-value is the proportion of R_i scores
#' greater than or equal to R_{k+1}.
#'
#' @param xy_sample Data frame containing one observation per subject
#' @param model_formula Linear model formula for mu.hat which will be fit
#' on augmented sample
#' @param X_new Covariate information for new observation
#' @param Y_new Hypothetical outcome for new observation
#'
#' @return p-value for new observation (X_new, Y_new)
#'
#' @export
sup_get_pval <- function(xy_sample, model_formula, X_new, Y_new) {

  # Rename Y_new to agree with LHS of model_formula
  xy_new <- cbind(X_new, Y_new)
  colnames(xy_new)[colnames(xy_new) == "Y_new"] <- formula.tools::lhs.vars(model_formula)

  # Only save xy_sample columns needed for model_formula
  xy_sample <- xy_sample %>%
    dplyr::select(formula.tools::lhs.vars(model_formula),
                  formula.tools::rhs.vars(model_formula))

  # Combine (X_new, Y_new) and xy_sample
  xy_aug <- rbind(xy_new, xy_sample)

  # Fit model with augmented sample
  lm.fit <- lm(formula = model_formula, data = xy_aug)
  R <- abs(lm.fit$residuals)

  # Get p-value
  pval <- sum(R >= R[1]) / length(R)

  return(pval)
}
