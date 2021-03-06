#' Supervised CDF pooling method with sample splitting
#'
#' @description Construct CDF pooling prediction interval at the covariates
#' of a new subject. This involves fitting a model mu.hat on the model fitting
#' subset of subjects and fitting residuals on the remaining subjects.
#' Compute quantiles q.hat(1-alpha) on the average empirical CDFs of the
#' residuals. At new covariates x, the prediction interval is
#' \[mu.hat(x) - q.hat(1-alpha), mu.hat(x) + q.hat(1-lapha)\].
#'
#' @param xy_data Data frame containing observations and outcomes for all
#' subjects. Must include a Subject column that identifies subjects.
#' @param model_formula Linear model formula for mu.hat which will be fit
#' on all subjects in model fitting set
#' @param alpha Significance level
#' @param k_val Number of subjects
#' @param k_model_fit Indices of subjects to use for model fitting
#' @param k_resid_fit Indices of subjects to use for residual fitting
#' @param new_xy_data Covariate and outcome data for new subject
#'
#' @return List containing prediction interval size, prediction interval
#' lower bound at new observation's covariate values, prediction interval
#' upper bound at new observation's covariate values, and whether new
#' observation's outcome is contained inside prediction interval.
#'
#' @export
sup_pool_cdfs_split <- function(xy_data, model_formula, alpha, k_val,
                                k_model_fit, k_resid_fit, new_xy_data) {

  # If k >= 2, fit model using observations in k_model_fit groups.
  # If k = 1, fit model without using any observed data. (Generate random data.)
  if(k_val >= 2) {

    xy_model_fit <- xy_data[xy_data$Subject %in% k_model_fit, ]
    model_fit <- lm(formula = model_formula, data = xy_model_fit)

  } else if(k_val == 1) {

    X_model_fit <- rnorm(1, 0, 1)
    Y_model_fit <- rnorm(1, 0, 1)
    model_fit <- lm(Y_model_fit ~ X_model_fit - 1)

  }

  # Get residuals on remaining groups
  xy_resid_fit <- xy_data[xy_data$Subject %in% k_resid_fit, ]

  resid_fit <-
    lapply(k_resid_fit,
           FUN = function(i) abs(xy_resid_fit[xy_resid_fit$Subject == i, ] %>%
                                   dplyr::select(formula.tools::lhs(model_formula)) -
                                   predict(model_fit,
                                           newdata = xy_resid_fit[xy_resid_fit$Subject == i, ])))

  # Use root solver to get threshold based on residuals.
  # Approx. minimum threshold where avg_Fhat >= 1-alpha.
  threshold_solved <-
    uniroot(f = function(x) sup_get_avg_Fhat(threshold = x,
                                             resid_fit = resid_fit) -
                                          (1 - alpha - 0.0001),
            lower = 0, upper = 1,
            extendInt = "yes")$root

  # Store size of interval
  pred_int_size <- 2*threshold_solved

  # Get prediction at new observation
  predict_Y <- predict(model_fit, newdata = new_xy_data)

  # Get bounds of prediction interval
  lower_bound <- predict_Y - threshold_solved
  upper_bound <- predict_Y + threshold_solved

  # If we observe Y_new, check whether (X_new, Y_new) is inside interval
  Y_new_observed <- as.character(formula.tools::lhs(model_formula)) %in%
    colnames(new_xy_data)

  if(Y_new_observed == FALSE) {

    covered <- NA

  } else {

    # Extract Y_new
    Y_new <- new_xy_data %>% dplyr::select(formula.tools::lhs(model_formula))

    # Check whether new observation is inside interval
    covered <- (lower_bound <= Y_new) & (Y_new <= upper_bound)

  }

  # Return prediction interval, interval size, and whether new (X, Y) is covered
  return(list(pred_int_size = pred_int_size,
              lower_bound = lower_bound,
              upper_bound = upper_bound,
              covered = covered))

}
