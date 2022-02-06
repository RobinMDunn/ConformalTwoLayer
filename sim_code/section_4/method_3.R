# Get supervised conformal interval using a subsample of one obs per group.
# theta_1, ..., theta_k ~ N(mu, tau^2)
# X_j1, ..., X_jn_j ~ N(0, 1)
# Y_{ji} = theta_j*X_{ji} + epsilon_{ji}, epsilon_{ji} ~ N(0, sigma^2)

library(R.utils)
library(progress)
library(data.table)
library(ConformalTwoLayer)

# Read in arguments for start/end k (number of groups),
# n (number of observations per group),
# mu and tau^2 (mean and variance of theta params).
start_k <- 20
end_k <- 1000
n_val <- 100
mu_val <- 0
tau_sq_val <- 1

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_k <- args[1]
  end_k <- args[2]
  n_val <- args[3]
  mu_val <- args[4]
  tau_sq_val <- args[5]
}

# Set alpha level
alpha <- 0.1

# Construct vectors of k values and n values
all_k <- c(seq(5, 100, by = 5), seq(200, 1000, by = 100))

k_vec <- all_k[start_k <= all_k & all_k <= end_k]

# Construct data frame to store results
results <- data.table(k = k_vec,
                      n = n_val,
                      mu = mu_val,
                      tau_sq = tau_sq_val,
                      coverage_2alpha = NA_real_,
                      avg_size_2alpha = NA_real_)

# Number of simulations to perform at each combination of k and n
n_sim <- 1000

# Number of times to resample to get average p-value
n_resamp <- 100

# For each combination of k and n, repeat n_sim times:
# Simulate data, construct prediction interval,
# check whether new observation is inside interval.
for(row in 1:nrow(results)) {

  # Vectors to store coverage and prediction interval lengths
  covered_2alpha <- rep(NA, n_sim)

  pred_int_size_2alpha <- rep(NA, n_sim)

  # Extract k and n
  k_val <- results[row, k]

  n_val <- results[row, n]

  # Keep coverage and length as NA unless k > 1/alpha - 1
  if(k_val > 1 / alpha - 1) {

    # Set up progress bar
    pb <- progress_bar$new(format = "Row :current / :total [:bar] :eta",
                           total = n_sim, clear = T, show_after = 0)

    for(sim in 1:n_sim) {

      # Increment progress bar
      pb$tick()

      # Set seed
      set.seed(sim)

      # Simulate data
      xy_data <- sup_generate_data(k = k_val, n = n_val,
                                   mu = mu_val, tau_sq = tau_sq_val,
                                   sigma_sq = 1)

      # Generate new X and Y observation
      new_xy_data <- sup_generate_data(k = 1, n = 1, mu = mu_val,
                                       tau_sq = tau_sq_val, sigma_sq = 1)

      # Get prediction interval size and whether new (X, Y) is covered
      sup_rep_sub_results <-
        sup_repeated_subsample(xy_data = xy_data,
                               model_formula = formula(Y ~ X1 - 1),
                               alpha = alpha, n_val = n_val,
                               k_indices = 1:k_val, n_resamp = n_resamp,
                               grid_values = seq(-10, 10, by = 1),
                               new_xy_data = new_xy_data)

      covered_2alpha[sim] <- sup_rep_sub_results$covered

      pred_int_size_2alpha[sim] <- sup_rep_sub_results$pred_int_size

    }

    # Store coverage proportion
    results[row, coverage_2alpha := mean(covered_2alpha)]

    # Store average prediction interval size
    results[row, avg_size_2alpha := mean(pred_int_size_2alpha)]

  }

}

# Save simulation results. Label with k, n, mu, and tau_sq values.
fwrite(results,
       file = paste0("sim_data/section_4/method_3_k_",
                     start_k, "_n_", n_val, "_mu_", mu_val,
                     "_tausq_", tau_sq_val, ".csv"))
