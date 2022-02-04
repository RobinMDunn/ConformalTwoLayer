# Estimate coverage of supervised conformal interval using repeated subsamples
# of one observation per group.
# To estimate coverage, rrepeat this process once for each row:
# - Leave out one row as the test point.
# - Leave out all observations from the individual associated w/ that row.
# - Fit model on remaining data (one obs per person).
# - Check whether model covers the reaction time in the held-out point.
# Repeat this entire procedure n_sim times to get n_sim estimates of coverage.
# Data example: sleepstudy from lme4

suppressMessages(library(progress))
suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(lme4))
suppressMessages(library(R.utils))
library(devtools)
load_all()

# Read in arguments for start/end n_sim and alpha (0.1, 0.15, 0.2)
start_n_sim <- 1
end_n_sim <- 1000
alpha <- 0.1

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  args <- as.numeric(args)
  start_n_sim <- args[1]
  end_n_sim <- args[2]
  alpha <- args[3]
}

# Number of times to resample to get average p-value
n_resamp <- 100

# Create vector of simulation indices
sim_vec <- start_n_sim:end_n_sim

# Read in data
data(sleepstudy)

sleep_df <- sleepstudy %>%
  mutate(Subject = as.numeric(Subject)) %>%
  as.data.table(key = "Subject")

# Add baseline (day 0) reaction time column
sleep_df[, Baseline := sleep_df[Days == 0][Subject, Reaction]]

sleep_df <- sleep_df[Days > 0]

# Number of rows to check coverage (hold out each observation once)
n_rows <- nrow(sleep_df) # 162

# Vector to store coverage in simulations
coverage_vec_2alpha <- rep(NA, n_rows)

# Construct data frame to store results
results <- data.table(sim = sim_vec,
                      alpha = alpha,
                      n_rows = n_rows,
                      coverage = NA_real_)

# Set up progress bar
pb <- progress_bar$new(format = paste0("Sim :current / :total",
                                       "[:bar] :eta"),
                       total = length(sim_vec), clear = T, show_after = 0)

# Get coverage over 162 rows repeatedly. (Method has inherent randomness.)
for(sim_index in sim_vec) {

  # Increment progress bar
  pb$tick()

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
  results[sim == sim_index, coverage := mean(coverage_vec_2alpha)]

}

# Save simulation results.
fwrite(results, file = paste0("sim_data/section_6/method_3_coverage_point",
                              as.integer(alpha*100), "_sim_", start_n_sim, "_",
                              end_n_sim, ".csv"))
