# Simulate unsupervised method 3, for case where we have k groups
# and n observations per group.
#
#
# DATA GENERATION:
# The observation from group j (j = 1, ..., k) is distributed as
#    Y_ji ~ Beta(theta_j, 1) (i = 1, ..., n_j)
#    theta_j ~ Exp(1)
# Vary k from 5 to 100 in increments of 5, and 200 to 1000 in increments of 100.
# Let n = 100.

library(R.utils)
library(progress)
library(data.table)
library(ConformalTwoLayer)

# Read in arguments for start/end k (number of groups),
# n (number of observations per group)
start_k <- 5
end_k <- 1000
n <- 100

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_k <- args[1]
  end_k <- args[2]
  n <- args[3]
}

# Set alpha level
alpha <- 0.1

# Construct vectors of k values, n values values
all_k <- c(seq(5, 100, by = 5), seq(200, 1000, by = 100))

k_vec <- all_k[start_k <= all_k & all_k <= end_k]

# Construct data frame to store results
results <- data.table(k = k_vec,
                      n = n,
                      coverage_2alpha = NA_real_,
                      avg_length_2alpha = NA_real_)

# Number of simulations to perform at each combination of k, n
n_sim <- 1000

# Number of times to resample to get average p-value
n_resamp <- 100

# Set up progress bar
pb <- progress_bar$new(format = paste0("[:bar] sim :current / :total :eta"),
                       total = n_sim * nrow(results), clear = T, show_after = 0)

# For each combination of k, n, repeat n_sim times:
# Simulate data, construct prediction interval,
# check whether new observation is inside interval.
for(row in 1:nrow(results)) {

  # Vectors to store coverage and prediction interval lengths
  covered_2alpha <- rep(NA, n_sim)

  pi_length_2alpha <- rep(NA, n_sim)

  # Extract k, n
  k_val <- results[row, k]

  n_val <- results[row, n]

  # Set seed - depends on k, n
  set.seed(10 + k_val + n_val)

  # Keep coverage and length as NA unless k > 2/alpha - 1
  if(k_val > 2 / alpha - 1) {

    for(sim in 1:n_sim) {

      # Increment progress bar
      pb$tick()

      # Draw theta parameter for each of the k groups
      theta <- rexp(n = k_val, rate = 1)

      # Generate n observations for each of the k groups
      Y <- vector("list", k_val)

      for(i in 1:k_val) {
        Y[[i]] <- rbeta(n = n_val, shape1 = theta[i], shape2 = 1)
      }

      # Generate a single new observation from a new group
      new_theta <- rexp(n = 1, rate = 1)

      new_Y <- rbeta(n = 1, shape1 = new_theta, shape2 = 1)

      # Construct prediction interval
      unsup_repeated_results <-
        unsup_repeated_subsample(Y = Y, alpha = alpha, k_val = k_val,
                                 n_resamp = n_resamp, new_Y = new_Y)

      # Check whether new observation is inside interval
      covered_2alpha[sim] <- unsup_repeated_results$covered

      # Store length of interval
      pi_length_2alpha[sim] <- unsup_repeated_results$pred_int_size

    }

    # Store coverage proportion
    results[row, coverage_2alpha := mean(covered_2alpha)]

    # Store average prediction interval length
    results[row, avg_length_2alpha := mean(pi_length_2alpha)]

  }

}

# Save simulation results.
fwrite(results,
       file = paste0("sim_data/appendix/unsup_exp_beta/method_3_k_",
                     start_k, "_n_", n, ".csv"))
