# Simulate unsupervised method 3, for case where we have k groups
# and n observations per group.
#
#
# DATA GENERATION:
# The observation from group j (j = 1, ..., k) is distributed as
#    Y_ji = theta_j + epsilon_ji (i = 1, ..., n)
#    theta_j ~ N(0, tau^2)
#    epsilon_ji ~ N(0, 1)
# Let tau^2 = 1
# Vary k from 5 to 100 in increments of 5, and 200 to 1000 in increments of 100.
# Let n = 40, 100, 1000.

library(R.utils)
library(progress)
library(data.table)
library(ConformalTwoLayer)

# Read in arguments for start/end k (number of groups),
# start/end n (number of observations per group),
# and tau^2 (variance of random effects distribution).
start_k <- 5
end_k <- 1000
start_n <- 40
end_n <- 1000
tau_sq <- 1

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_k <- args[1]
  end_k <- args[2]
  start_n <- args[3]
  end_n <- args[4]
  tau_sq <- args[5]
}

# Set alpha level
alpha <- 0.1

# Construct vectors of k values, n values, and tau_sq values
all_k <- c(seq(5, 100, by = 5), seq(200, 1000, by = 100))

k_vec <- all_k[start_k <= all_k & all_k <= end_k]

all_n <- c(40, 100, 1000)

n_vec <- all_n[start_n <= all_n & all_n <= end_n]

# Construct data frame to store results
results <- data.table(expand.grid(k_vec, n_vec),
                      tau_sq = tau_sq,
                      coverage_2alpha = NA_real_,
                      avg_length_2alpha = NA_real_)

setnames(results, old = c("Var1", "Var2"), new = c("k", "n"))

# Number of simulations to perform at each combination of k, n, and tau_sq
n_sim <- 1000

# Number of times to resample to get average p-value
n_resamp <- 100

# Set up progress bar
pb <- progress_bar$new(format = paste0("[:bar] sim :current / :total :eta"),
                       total = n_sim * nrow(results), clear = T, show_after = 0)

# For each combination of k, n, and tau_sq, repeat n_sim times:
# Simulate data, construct prediction interval,
# check whether new observation is inside interval.
for(row in 1:nrow(results)) {

  # Vectors to store coverage and prediction interval lengths
  covered_2alpha <- rep(NA, n_sim)

  pi_length_2alpha <- rep(NA, n_sim)

  # Extract k, n, and tau_sq
  k_val <- results[row, k]

  n_val <- results[row, n]

  tau_sq_val <- results[row, tau_sq]

  # Set seed - depends on k, n, and tau_sq
  set.seed(tau_sq_val * 10 + k_val + n_val)

  # Keep coverage and length as NA unless k > 2/alpha - 1
  if(k_val > 2 / alpha - 1) {

    for(sim in 1:n_sim) {

      # Increment progress bar
      pb$tick()

      # Simulate data
      Y <- unsup_generate_data(k = k_val, n_vec = rep(n_val, times = k_val),
                               mu = 0, tau_sq = tau_sq_val)

      # Generate a single new observation from a new group
      new_Y <- as.numeric(unsup_generate_data(k = 1, n_vec = 1,
                                              mu = 0, tau_sq = tau_sq_val))

      # Construct prediction set and check whether new_Y is in set
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

# Save simulation results. Label with start k, start n, and tau_sq values.
fwrite(results, file =  paste0("sim_data/appendix/unsup_addl_normal/method_3_k_",
                               start_k, "_n_", start_n,
                               "_tausq_", tau_sq, ".csv"))
