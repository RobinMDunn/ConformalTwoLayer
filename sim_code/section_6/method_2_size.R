# Get supervised conformal interval using one observation per group
# Data example: sleepstudy from lme4

suppressMessages(library(progress))
suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(lme4))
library(devtools)
load_all()

# Set alpha level
alpha <- 0.10

# Read in data
data(sleepstudy)

sleep_df <- sleepstudy %>%
  mutate(Subject = as.numeric(Subject)) %>%
  as.data.table(key = "Subject")

# Add baseline (day 0) reaction time column
sleep_df[, Baseline := sleep_df[Days == 0][Subject, Reaction]]

sleep_df <- sleep_df[Days > 0]

# Construct vectors of values for making predictions
day_vec <- c(1, 5, 9)

baseline_vec <- seq(190, 330, by = 10)

# Construct data frame to store results
results <- data.table(expand.grid(Days = day_vec, Baseline = baseline_vec),
                      alpha = alpha,
                      min_pred_int = NA_real_,
                      max_pred_int = NA_real_)

# Set up progress bar
pb <- progress_bar$new(format = paste0("Row :current / :total",
                                       "[:bar] :eta"),
                       total = nrow(results), clear = T, show_after = 0)

# For each combination of Days and Baseline, construct prediction interval.
for(row in 1:nrow(results)) {

  # Increment progress bar
  pb$tick()

  # Extract day and baseline measure. Use as new_xy_data.
  day_val <- results[row, Days]

  baseline_val <- results[row, Baseline]

  new_xy_data <- data.frame(Days = day_val, Baseline = baseline_val)

  # Set seed - depends on day and baseline
  set.seed(day_val + baseline_val)

  # n_val is number of observations for each individual (same across indivs)
  n_val <- nrow(sleep_df[Subject == 1])

  # Get prediction interval results
  single_sub_results <-
    sup_single_subsample(xy_data = sleep_df,
                         model_formula = formula(Reaction ~ Days + Baseline - 1),
                         alpha = alpha,
                         n_val = n_val,
                         k_indices = unique(sleep_df[, Subject]),
                         grid_values = seq(min(sleep_df$Reaction) - 50,
                                           max(sleep_df$Reaction) + 50,
                                           length.out = 20),
                         new_xy_data = new_xy_data)

  # Store prediction interval bounds
  results[row, min_pred_int := single_sub_results$lower_bound]

  # Store average prediction interval length
  results[row, max_pred_int := single_sub_results$upper_bound]

}

# Save simulation results.

fwrite(results, file = "sim_data/section_6/method_2_size.csv")
