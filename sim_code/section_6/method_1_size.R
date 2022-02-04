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

  ##### Get conformal prediction interval #####

  # Fit model on pooled data from half of the individuals
  k_model_fit <- sample(x = unique(sleep_df$Subject),
                        size = floor(0.5*length(unique(sleep_df$Subject))),
                        replace = FALSE)

  # Indices for residual fitting
  k_resid_fit <- setdiff(unique(sleep_df$Subject), k_model_fit)

  # Get prediction interval results
  pool_cdf_results <-
    sup_pool_cdfs_split(xy_data = sleep_df,
                        model_formula = formula(Reaction ~ Days + Baseline - 1),
                        alpha = alpha,
                        k_val = length(unique(sleep_df$Subject)),
                        k_model_fit = k_model_fit,
                        k_resid_fit = k_resid_fit,
                        new_xy_data = new_xy_data)

  # Store prediction interval bounds
  results[row, min_pred_int := pool_cdf_results$lower_bound]

  # Store average prediction interval length
  results[row, max_pred_int := pool_cdf_results$upper_bound]

}

# Save simulation results.

fwrite(results, file = "sim_data/section_6/method_1_size.csv")
