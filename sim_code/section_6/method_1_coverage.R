# Estimate coverage of supervised conformal interval using CDF pooling.
# To estimate coverage, repeat this process once for each row:
# - Leave out one row as the test point.
# - Leave out all observations from the individual associated w/ that row.
# - Fit model on remaining data.
# - Check whether model covers the reaction time in the held-out point.
# Data example: sleepstudy from lme4

library(progress)
library(tidyverse)
library(data.table)
library(lme4)
library(ConformalTwoLayer)

# Read in start/end alpha levels (0.10, 0.15, 0.20)
alpha_start <- 0.1
alpha_end <- 0.2

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  alpha_start <- args[1]
  alpha_end <- args[2]
}

# Create alpha vector
alpha_all <- c(0.10, 0.15, 0.20)
alpha_vec <- alpha_all[alpha_start <= alpha_all & alpha_all <= alpha_end]

# Set number of simulations
n_sim <- 1000

# Read in data
data(sleepstudy)

sleep_df <- sleepstudy %>%
  dplyr::mutate(Subject = as.numeric(Subject)) %>%
  as.data.table(key = "Subject")

# Add baseline (day 0) reaction time column
sleep_df[, Baseline := sleep_df[Days == 0][Subject, Reaction]]

sleep_df <- sleep_df[Days > 0]

# Number of rows to check coverage (hold out each observation once)
n_rows <- nrow(sleep_df) # 162

# Vector to store coverage in simulations
coverage_vec <- rep(NA, n_rows)

# Construct data frame to store results
results <- data.table(expand.grid(sim = 1:n_sim,
                                  alpha = alpha_vec),
                      n_rows = n_rows,
                      coverage = NA_real_)

# Set up progress bar
pb <- progress_bar$new(format = paste0("Sim :current / :total",
                                       "[:bar] :eta"),
                       total = nrow(results), clear = T, show_after = 0)

# Get coverage over 162 rows repeatedly. (Method has inherent randomness.)
for(results_row in 1:nrow(results)) {

  # Increment progress bar
  pb$tick()

  # Extra sim_index and alpha
  sim_index <- results[results_row, sim]
  alpha <- results[results_row, alpha]

  # Set seed
  set.seed(10000*alpha + sim_index)

  # Check coverage of prediction interval in n_rows simulations.
  for(row in 1:n_rows) {

    # Select row to predict.
    # Leave out individual connected to row.
    held_out_row <- sleep_df[row]
    held_out_indiv <- sleep_df[row, Subject]

    # Fit model on pooled data from half of the individuals
    k_model_fit <- sample(x = setdiff(unique(sleep_df$Subject), held_out_indiv),
                          size = floor(0.5*(length(unique(sleep_df$Subject))-1)),
                          replace = FALSE)

    # Indices for residual fitting
    k_resid_fit <- setdiff(unique(sleep_df$Subject),
                           c(k_model_fit, held_out_indiv))

    # Get prediction interval results
    pool_cdf_results <-
      sup_pool_cdfs_split(xy_data = sleep_df[Subject != held_out_indiv],
                          model_formula = formula(Reaction ~ Days + Baseline - 1),
                          alpha = alpha,
                          k_val = length(unique(sleep_df$Subject)) - 1,
                          k_model_fit = k_model_fit,
                          k_resid_fit = k_resid_fit,
                          new_xy_data = held_out_row)

    # Check if held-out observation is in prediction interval
    coverage_vec[row] <- pool_cdf_results$covered

  }

  # Get overall coverage
  results[results_row, coverage := mean(coverage_vec)]

}

# Save simulation results.
fwrite(results, file = paste0("sim_data/section_6/method_1_coverage_point",
                              as.integer(alpha_start*100), ".csv"))
