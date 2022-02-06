# Simulate unsupervised method 0, for case where we have k groups
# and n observations per group.
#
# DATA GENERATION:
# The observation from group j (j = 1, ..., k) is distributed as
#    Y_ji = theta_j + epsilon_ji (i = 1, ..., n)
#    theta_j ~ N(0, tau^2)
#    epsilon_ji ~ N(0, 1)
# Let tau^2 = 1
# Vary k from 5 to 100 in increments of 5, and 200 to 1000 in increments of 100.
# Let n = 100

library(R.utils)
library(progress)
library(data.table)
library(ConformalTwoLayer)

# Read in arguments for start/end k (number of groups),
# n (number of observations per group),
# and tau^2 (variance of random effects distribution).
start_k <- 5
end_k <- 1000
n <- 100
tau_sq <- 1

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_k <- args[1]
  end_k <- args[2]
  n <- args[3]
  tau_sq <- args[4]
}

# Set alpha level
alpha <- 0.1

# Construct vectors of k values, n values, and tau_sq values
all_k <- c(seq(5, 100, by = 5), seq(200, 1000, by = 100))

k_vec <- all_k[start_k <= all_k & all_k <= end_k]

# Construct data frame to store results
results <- data.table(k = k_vec,
                      n = n,
                      tau_sq = tau_sq,
                      coverage = NA_real_,
                      avg_length = NA_real_)

# Number of simulations to perform at each combination of k, n, and tau_sq
n_sim <- 1000

# Vectors to store coverage and prediction interval lengths
covered <- rep(NA, n_sim)

pred_int_length <- rep(NA, n_sim)

# Set up progress bar
pb <- progress_bar$new(format = paste0("[:bar] sim :current / :total"),
                       total = n_sim * nrow(results), clear = T, show_after = 0)

# For each combination of k, n, and tau_sq, repeat n_sim times:
# Simulate data, construct prediction interval,
# check whether new observation is inside interval.
for(row in 1:nrow(results)) {

  # Extract k, n, and tau_sq
  k_val <- results[row, k]

  n_val <- results[row, n]

  tau_sq_val <- results[row, tau_sq]

  # Set seed - depends on k, n, and tau_sq
  set.seed(tau_sq_val * 10 + k_val + n_val)

  # Keep coverage and length as NA unless k >= 4/alpha - 1 and n_j >= 4/alpha - 1
  if((k_val >= 4 / alpha - 1) & (n_val >= 4 / alpha - 1)) {

    for(sim in 1:n_sim) {

      # Increment progress bar
      pb$tick()

      # Simulate data
      Y <- unsup_generate_data(k = k_val, n_vec = rep(n_val, times = k_val),
                               mu = 0, tau_sq = tau_sq_val)

      # Generate a single new observation from a new group
      new_Y <- as.numeric(unsup_generate_data(k = 1, n_vec = 1, mu = 0,
                                              tau_sq = tau_sq_val))

      # Prediction interval for new observation
      unsup_double_results <-
        unsup_double_conformal(Y = Y, alpha = alpha, n_val = n_val,
                               new_Y = new_Y)

      # Check whether new observation is inside interval
      covered[sim] <- unsup_double_results$covered

      # Store length of interval
      pred_int_length[sim] <- unsup_double_results$pred_int_size
    }

    # Store coverage proportion
    results[row, coverage := mean(covered)]

    # Store average prediction interval length
    results[row, avg_length := mean(pred_int_length)]
  }

}

# Save simulation results. Label with start k, n, tau_sq values.
fwrite(results, file =  paste0("sim_data/section_3/balanced/method_0_k_",
                               start_k, "_n_", n,
                               "_tausq_", tau_sq, ".csv"))
