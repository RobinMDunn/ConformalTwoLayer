# Simulate unsupervised method 0, for case where we have k groups
# and n observations per group.
#
# DATA GENERATION:
# The observation from group j (j = 1, ..., k) is distributed as
#    Y_ji ~ Beta(theta_j, 1) (i = 1, ..., n_j)
#    theta_j ~ Exp(1)
# Vary k from 5 to 100 in increments of 5, and 200 to 1000 in increments of 100.
# Vary n from 5 to 100 in increments of 5, and 200 to 1000 in increments of 100.
#
# PREDICTION INTERVAL:
# 1. Apply order statistic method to each of the k groups to get k intervals
# [l_1, u_1], ..., [l_k, u_k] each at level 1-alpha/2.
# l_1 is the floor((n + 1) * alpha/4) order statistic of the first sample,
# u_1 is the ceiling((n + 1)*(1 - alpha/4)) order statistic of the first sample,
# and so on for the other samples.
# 2. Consider (l_1, ..., l_k). Take l_(r), where r = floor((k + 1) * alpha/4).
# Consider (u_1, ..., u_k). Take u_(s), where s = ceiling((k + 1)*(1 - alpha/4)).
# Prediction interval: C = [l_(r), u_(s)].
#
# RUNNING:
# When running from shell, you can optionally enter four arguments:
# start k, end k, start n, end n.

suppressMessages(library(R.utils))
suppressMessages(library(progress))
suppressMessages(library(data.table))
library(devtools)
load_all()

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

# Construct vectors of k values and n values values
all_k <- c(seq(5, 100, by = 5), seq(200, 1000, by = 100))

k_vec <- all_k[start_k <= all_k & all_k <= end_k]

# Construct data frame to store results
results <- data.table(k = k_vec,
                      n = n,
                      coverage = NA_real_,
                      avg_length = NA_real_)

# Number of simulations to perform at each combination of k and n
n_sim <- 1000

# Vectors to store coverage and prediction interval lengths
covered <- rep(NA, n_sim)

pred_int_length <- rep(NA, n_sim)

# Set up progress bar
pb <- progress_bar$new(format = paste0("sim :current / :total [:bar] :eta"),
                       total = n_sim * nrow(results), clear = T, show_after = 0)

# For each combination of k and n, repeat n_sim times:
# Simulate data, construct prediction interval,
# check whether new observation is inside interval.
for(row in 1:nrow(results)) {

  # Extract k and n
  k_val <- results[row, k]

  n_val <- results[row, n]

  # Set seed - depends on k and n
  set.seed(10 + k_val + n_val)

  # Keep coverage and length as NA unless k >= 4/alpha - 1 and n_j >= 4/alpha - 1
  if((k_val >= 4 / alpha - 1) & (n_val >= 4 / alpha - 1)) {

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

# Save simulation results.
fwrite(results,
       file = paste0("sim_data/appendix/unsup_exp_beta/method_0_k_",
                     start_k, "_n_", n, ".csv"))
