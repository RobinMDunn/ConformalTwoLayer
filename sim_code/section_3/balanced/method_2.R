# Simulate unsupervised method 2, for case where we have k groups
# and n observations per group.
#
# DATA GENERATION:
# The observation from group j (j = 1, ..., k) is distributed as
#    Y_ji = theta_j + epsilon_ji (i = 1, ..., n)
#    theta_j ~ N(0, tau^2)
#    epsilon_ji ~ N(0, 1)
# Vary tau^2 = {0.1, 1, 10}
# Vary k from 5 to 100 in increments of 5, and 200 to 1000 in increments of 100.
# Vary n from 5 to 100 in increments of 5, and 200 to 1000 in increments of 100.
#
# PREDICTION INTERVAL:
# Choose one observation from each of the k groups. Call this Y_subsample.
# Sort all observations in Y_subsample from low to high.
# The interval [Y_subsample_(r), Y_subsample_(s)] is the prediction interval
# for a new observation,
# where r = floor((k + 1) * alpha/2) and s = ceiling((k + 1)*(1 - alpha/2)).
#
# RUNNING:
# When running from shell, you can optionally enter six arguments:
# start k, end k, start n, end n, start tau^2, and end tau^2.

suppressMessages(library(R.utils))
suppressMessages(library(progress))
suppressMessages(library(data.table))
library(devtools)
load_all()

# Read in arguments for start/end k (number of groups),
# start/end n (number of observations per group),
# and start/end tau^2 (variance of random effects distribution).
start_k <- 5
end_k <- 1000
start_n <- 5
end_n <- 1000
start_tau_sq <- 0.1
end_tau_sq <- 10

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_k <- args[1]
  end_k <- args[2]
  start_n <- args[3]
  end_n <- args[4]
  start_tau_sq <- args[5]
  end_tau_sq <- args[6]
}

# Set alpha level
alpha <- 0.1

# Construct vectors of k values, n values, and tau_sq values
all_k <- c(seq(5, 100, by = 5), seq(200, 1000, by = 100))

k_vec <- all_k[start_k <= all_k & all_k <= end_k]

all_n <- c(seq(5, 100, by = 5), seq(200, 1000, by = 100))

n_vec <- all_n[start_n <= all_n & all_n <= end_n]

all_tau_sq <- c(0.1, 1, 10)

tau_sq_vec <- all_tau_sq[start_tau_sq <= all_tau_sq &
                           all_tau_sq <= end_tau_sq]

# Construct data frame to store results
results <- data.table(expand.grid(k_vec, n_vec, tau_sq_vec),
                      coverage = NA_real_,
                      avg_length = NA_real_)

setnames(results, old = c("Var1", "Var2", "Var3"), new = c("k", "n", "tau_sq"))

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

  # Keep coverage and length as NA unless k >= 2/alpha - 1
  if(k_val >= 2 / alpha - 1) {

    for(sim in 1:n_sim) {

      # Increment progress bar
      pb$tick()

      # Simulate data
      Y <- unsup_generate_data(k = k_val, n_vec = rep(n_val, times = k_val),
                               tau_sq = tau_sq_val)

      # Generate a single new observation from a new group
      new_Y <- as.numeric(unsup_generate_data(k = 1, n_vec = 1,
                                              tau_sq = tau_sq_val))

      # Construct prediction interval
      unsup_single_sub_results <-
        unsup_single_subsample(Y = Y, alpha = alpha, k_val = k_val,
                               new_Y = new_Y)

      # Check whether new observation is inside interval
      covered[sim] <- unsup_single_sub_results$covered

      # Store length of interval
      pred_int_length[sim] <- unsup_single_sub_results$pred_int_size

    }

    # Store coverage proportion
    results[row, coverage := mean(covered)]

    # Store average prediction interval length
    results[row, avg_length := mean(pred_int_length)]
  }

}

results
results

# # Save simulation results. Label with start/end k and tau_sq values.
# fwrite(results,
#        file = paste0("data/unsupervised/method_2/method_2_k_",
#                      start_k, "_", end_k,
#                      "_n_", start_n, "_", end_n,
#                      "_tausq_", start_tau_sq_text, "_",
#                      end_tau_sq_text, ".csv"))
