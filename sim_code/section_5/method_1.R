# Method 1: Use only group 1 data to predict a new observation from group 1.
# theta_1, ..., theta_k ~ N(0,1)
# Y_{ij} ~ N(theta_j, sd = sigma)

suppressMessages(library(R.utils))
suppressMessages(library(progress))
suppressMessages(library(data.table))
library(devtools)
load_all()
#library(ConformalRandomEffects)

# Read in arguments for start/end k (number of groups),
# start/end n (number of observations per group),
# and start/end sigma^2 (variance of individual distributions).
start_k <- 5
end_k <- 1000
n_j_val <- 20
sigma_sq_start <- 1
sigma_sq_end <- 100

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_k <- args[1]
  end_k <- args[2]
  n_j_val <- args[3]
  sigma_sq_start <- args[4]
  sigma_sq_end <- args[5]
}

# Set alpha level
alpha <- 0.1

#######################
##### Data set-up #####
#######################

# Vector of possible sigma_sq params
sigma_sq_vec <- c(1, 100)

# Vary k in increments of 5
k_vec <- seq(start_k, end_k, by = 5)

# Construct data frame to store results
results <- data.table(
  expand.grid(k = k_vec,
              sigma_sq = sigma_sq_vec[sigma_sq_vec >= sigma_sq_start &
                                        sigma_sq_vec <= sigma_sq_end]),
  n_j = n_j_val,
  coverage = NA_real_,
  avg_length = NA_real_) %>%
  dplyr::select(k, n_j, sigma_sq, coverage, avg_length)

# Number of simulations to perform at each combination of k, n, and sigma_sq
n_sim <- 1000

###########################
##### Run simulations #####
###########################

# Set up progress bar
pb <- progress_bar$new(format = "Row :current / :total [:bar] :eta",
                       total = nrow(results)*n_sim, clear = T, show_after = 0)

# For each combination of k, n, and sigma_sq, repeat n_sim times:
# Simulate data, construct prediction interval,
# check whether new observation is inside interval.
for(row in 1:nrow(results)){

  # Extract k, n, and sigma_sq
  k_val <- results[row, k]

  n_val <- results[row, n_j]

  sigma_sq_val <- results[row, sigma_sq]

  # Keep coverage and length as NA unless n_1 > 1/alpha - 1
  if(n_val > 1 / alpha - 1) {

    # Vectors to store coverage and prediction interval lengths
    covered <- rep(NA, n_sim)

    pred_int_size <- rep(NA, n_sim)

    # Run n_sim simulations for group 1 prediction interval
    for(sim in 1:n_sim) {

      pb$tick()

      # Generate data, including an additional observation from group 1
      Y <- unsup_generate_data(k = k_val,
                               n_vec = c(n_val + 1, rep(n_val, length = k_val - 1)),
                               mu = 0, tau_sq = 1, sigma_sq = sigma_sq_val)

      # Extract Y_new as final observation from group 1
      Y_new <- tail(Y[[1]], 1)

      # Remove Y_new from original group 1 sample
      Y[[1]] <- head(Y[[1]], n_val)

      # Get prediction interval size and whether new Y is covered
      unsup_group_1_results <- unsup_method_group_1(Y = Y, alpha = alpha,
                                                    Y_new = Y_new)

      covered[sim] <- unsup_group_1_results$covered

      pred_int_size[sim] <- unsup_group_1_results$pred_int_size
    }

    # Store coverage proportion
    results[row, coverage := mean(covered)]

    # Store average prediction interval length
    results[row, avg_length := mean(pred_int_size)]

  }

}

# Save simulation results. Label with start/end k and sigma_sq values.

fwrite(results, file = paste0("sim_data/section_5/method_1_k_", start_k,
                              "_n_", n_j_val, "_sigmasq_", sigma_sq_start,
                              ".csv"))
