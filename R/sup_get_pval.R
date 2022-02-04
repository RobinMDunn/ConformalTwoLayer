#' Function to get p-val
#'
#' @param X_sample
#' @param Y_sample
#' @param X_new
#' @param Y_new
#'
#' @return
#' @export
#'
#' @examples
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
