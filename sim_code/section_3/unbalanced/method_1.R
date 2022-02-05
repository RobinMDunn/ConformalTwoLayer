# Simulate unsupervised method 1, for case where we have k unbalanced groups.
# n_small = 5 observations for k-1 groups, n_large = 1000 observations for 1 group.
# Compute eCDFs. Get lower and upper 5% bounds from averaging eCDFs.
#
# DATA GENERATION:
# The observation from group j (j = 1, ..., k) is distributed as
#    Y_ji = theta_j + epsilon_ji (i = 1, ..., n_j)
#    theta_j ~ N(0, tau^2 = 100)
#    epsilon_ji ~ N(0, sigma^2 = 0.1)
# Vary k from 20 to 100 in increments of 5.
#
# PREDICTION INTERVAL:
# Sort all observations (Y) from low to high.
# The interval [Y_(r), Y_(s)] is the prediction interval for a new observation,
# where r = floor((N + 1) * alpha/2) and s = ceiling((N + 1)*(1 - alpha/2)).
#
# RUNNING:
# When running from shell, you can optionally enter two arguments:
# start k, end k.

suppressMessages(library(R.utils))
suppressMessages(library(progress))
suppressMessages(library(data.table))
library(devtools)
load_all()

# Read in arguments for start/end k (number of groups) and
# number of observations for small and large groups
start_k <- 5
end_k <- 100
n_small <- 5
n_large <- 1000

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_k <- args[1]
  end_k <- args[2]
  n_small <- args[3]
  n_large <- args[4]
}

# Set alpha level
alpha <- 0.1

# Construct vector of k values
all_k <- seq(5, 100, by = 5)

k_vec <- all_k[start_k <= all_k & all_k <= end_k]

# Construct data frame to store results
results <- data.table(k = k_vec,
                      n_small = n_small,
                      n_large = n_large,
                      coverage = NA_real_,
                      avg_length = NA_real_)

# Number of simulations to perform at each value of k
n_sim <- 1000

# Vectors to store coverage and prediction interval lengths
covered <- rep(NA, n_sim)

pred_int_length <- rep(NA, n_sim)

# Set up progress bar
pb <- progress_bar$new(format = paste0("[:bar] sim :current / :total :eta"),
                       total = n_sim * length(k_vec), clear = T,
                       show_after = 0)

# For each k, repeat n_sim times:
# Simulate data, construct prediction interval,
# check whether new observation is inside interval.
for(row in 1:nrow(results)) {

  # Extract k
  k_val <- results[row, k]

  for(sim in 1:n_sim) {

    # Increment progress bar
    pb$tick()

    # Simulate data
    Y <- unsup_generate_data(k = k_val,
                             n_vec = c(rep(n_small, times = k_val - 1), n_large),
                             tau_sq = 100, sigma_sq = 0.1)

    # Generate a single new observation from a new group
    new_Y <- unsup_generate_data(k = 1, n_vec = 1, tau_sq = 100, sigma_sq = 0.1)

    # Prediction interval for new observation
    unsup_pool_results <- unsup_pool_cdfs(Y = Y, alpha = alpha, new_Y = new_Y)

    # Check whether new observation is inside interval
    covered[sim] <- unsup_pool_results$covered

    # Store length of interval
    pred_int_length[sim] <- unsup_pool_results$pred_int_size

  }

  # Store coverage proportion
  results[row, coverage := mean(covered)]

  # Store average prediction interval length
  results[row, avg_length := mean(pred_int_length)]

}

# Save simulation results. Label with n_small, n_large, k values.
fwrite(results,
       file = paste0("sim_data/section_3/unbalanced/method_1_small_",
                     n_small, "_large_", n_large, "_",
                     start_k, "_", end_k, ".csv"))
