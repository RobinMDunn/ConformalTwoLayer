# Get supervised conformal interval using naive pooling
# theta_1, ..., theta_k ~ N(mu, tau^2)
# X_j1, ..., X_jn_j ~ N(0, 1)
# Y_{ji} = theta_j*X_{ji} + epsilon_{ji}, epsilon_{ji} ~ N(0, sigma^2)

library(R.utils)
library(progress)
library(data.table)
library(ConformalTwoLayer)

# Read in arguments for start/end k (number of groups),
# start/end n (number of observations per group),
# mu and tau^2 (mean and variance of theta params).
# Use (mu_val, tau_sq_val) = (0, 1) or (mu_val, tau_sq_val) = (1, 0.1)
start_k <- 20
end_k <- 1000
start_n <- 20
end_n <- 1000
mu_val <- 0
tau_sq_val <- 1

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_k <- args[1]
  end_k <- args[2]
  start_n <- args[3]
  end_n <- args[4]
  mu_val <- args[5]
  tau_sq_val <- args[6]
}

# Set alpha level
alpha <- 0.1

# Construct vectors of k values and n values
all_k <- c(seq(5, 100, by = 5), seq(200, 1000, by = 100))

k_vec <- all_k[start_k <= all_k & all_k <= end_k]

all_n <- c(20, 100, 1000)

n_vec <- all_n[start_n <= all_n & all_n <= end_n]

# Construct data frame to store results
results <- data.table(expand.grid(k_vec, n_vec),
                      mu = mu_val,
                      tau_sq = tau_sq_val,
                      coverage = NA_real_,
                      avg_size = NA_real_)

setnames(results, old = c("Var1", "Var2"), new = c("k", "n"))

# Number of simulations to perform at each combination of k and n
n_sim <- 1000

# Vectors to store coverage and prediction interval lengths
covered <- rep(NA, n_sim)

pred_int_size <- rep(NA, n_sim)

# For each combination of k and n, repeat n_sim times:
# Simulate data, construct prediction interval,
# check whether new observation is inside interval.
for(row in 1:nrow(results)) {

  # Extract k and n
  k_val <- results[row, k]

  n_val <- results[row, n]

  # Set up progress bar
  pb <- progress_bar$new(format = "Row :current / :total [:bar] :eta",
                         total = n_sim, clear = T, show_after = 0)

  for(sim in 1:n_sim) {

    # Increment progress bar
    pb$tick()

    # Simulate data
    xy_data <- sup_generate_data(k = k_val, n = n_val,
                                 mu = mu_val, tau_sq = tau_sq_val,
                                 sigma_sq = 1)

    # Fit model on pooled data from half of the groups
    k_model_fit <- sample(x = k_val, size = floor(k_val/2), replace = FALSE)

    # Fit residuals on remaining groups
    k_resid_fit <- setdiff(1:k_val, k_model_fit)

    # Generate new X and Y observation
    new_xy_data <- sup_generate_data(k = 1, n = 1, mu = mu_val,
                                     tau_sq = tau_sq_val, sigma_sq = 1)

    # Get prediction interval size and whether new (X, Y) is covered
    sup_pool_cdf_results <-
      sup_pool_cdfs_split(xy_data = xy_data,
                          model_formula = formula(Y ~ X1 - 1),
                          alpha = alpha,
                          k_val = k_val,
                          k_model_fit = k_model_fit,
                          k_resid_fit = k_resid_fit,
                          new_xy_data = new_xy_data)

    covered[sim] <- sup_pool_cdf_results$covered

    pred_int_size[sim] <- sup_pool_cdf_results$pred_int_size

  }

  # Store coverage proportion
  results[row, coverage := mean(covered)]

  # Store average prediction interval length
  results[row, avg_size := mean(pred_int_size)]

}

# Save simulation results. Label with start/end k, n, mu, and tau_sq values.
fwrite(results,
       file = paste0("sim_data/appendix/sup_addl/method_1_k_",
                     start_k, "_", end_k,
                     "_n_", start_n, "_", end_n,
                     "_mu_", mu_val,
                     "_tausq_", tau_sq_val, ".csv"))
