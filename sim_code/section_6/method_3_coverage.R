# Estimate coverage of supervised conformal interval using repeated subsamples
# of one observation per group.
# To estimate coverage, rrepeat this process once for each row:
# - Leave out one row as the test point.
# - Leave out all observations from the individual associated w/ that row.
# - Fit model on remaining data (one obs per person).
# - Check whether model covers the reaction time in the held-out point.
# Repeat this entire procedure n_sim times to get n_sim estimates of coverage.
# Data example: sleepstudy from lme4

library(progress)
library(tidyverse)
library(data.table)
library(lme4)
library(ConformalTwoLayer)

# Read in arguments for start/end n_sim and start/end alpha (0.10, 0.15, 0.20)
start_n_sim <- 1
end_n_sim <- 1000
alpha_start <- 0.1
alpha_end <- 0.2

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_n_sim <- args[1]
  end_n_sim <- args[2]
  alpha_start <- args[3]
  alpha_end <- args[4]
}

# Create alpha vector
alpha_all <- c(0.10, 0.15, 0.20)
alpha_vec <- alpha_all[alpha_start <= alpha_all & alpha_all <= alpha_end]

# Number of times to resample to get average p-value
n_resamp <- 100

# Create vector of simulation indices
sim_vec <- start_n_sim:end_n_sim

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
coverage_vec_2alpha <- rep(NA, n_rows)

# Construct data frame to store results
results <- data.table(expand.grid(sim = sim_vec,
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

    # n_val is number of observations for each individual (same across indivs)
    n_val <- nrow(sleep_df[Subject == 1])

    # Get prediction interval results
    repeated_sub_results <-
      sup_repeated_subsample(xy_data = sleep_df[Subject != held_out_indiv],
                             model_formula = formula(Reaction ~ Days + Baseline - 1),
                             alpha = alpha,
                             n_val = n_val,
                             k_indices = setdiff(unique(sleep_df[, Subject]),
                                                 held_out_indiv),
                             n_resamp = n_resamp,
                             grid_values = seq(min(sleep_df$Reaction) - 50,
                                               max(sleep_df$Reaction) + 50,
                                               length.out = 20),
                             coverage_only = TRUE,
                             new_xy_data = held_out_row)

    # Check if held-out observation is in prediction intervals
    coverage_vec_2alpha[row] <- repeated_sub_results$covered

  }

  # Get overall coverage
  results[results_row, coverage := mean(coverage_vec_2alpha)]

}

# Save simulation results.
fwrite(results, file = paste0("sim_data/section_6/method_3_coverage_point",
                              as.integer(alpha_start*100), "_sim_",
                              start_n_sim, "_", end_n_sim, ".csv"))
