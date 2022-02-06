# Repeatedly simulate lower and upper bounds from methods 2 and 3
# on single dataset.

library(R.utils)
library(data.table)
library(tidyverse)
library(progress)
library(ConformalTwoLayer)

# Data frame to store results
pred_ints <- data.table(sim = 1:1000,
                        method_2_lb = NA_real_,
                        method_2_ub = NA_real_,
                        method_3_lb = NA_real_,
                        method_3_ub = NA_real_)

# Generate a single data set
set.seed(20211220)

# Simulate data
k_val <- 100
n_val <- 100
alpha <- 0.1
Y <- unsup_generate_data(k = k_val, n_vec = rep(n_val, times = k_val), mu = 0,
                         tau_sq = 1)

# Set up progress bar
pb <- progress_bar$new(format = paste0("sim :current / :total [:bar] :eta"),
                       total = nrow(pred_ints), clear = T, show_after = 0)

# Repeatedly simulate methods 2 and 3
for(i in 1:nrow(pred_ints)) {

  # Update progress bar
  pb$tick()

  ###### Construct method 2 prediction interval #####

  # Construct prediction interval from Y sample
  method2_pred_int <- unsup_single_subsample(Y = Y, alpha = alpha, k_val = k_val)

  # Store method 2 results
  pred_ints[sim == i, method_2_lb := method2_pred_int$lower_bound]
  pred_ints[sim == i, method_2_ub := method2_pred_int$upper_bound]

  ###### Construct method 3 prediction interval #####

  # Construct prediction interval from Y sample
  n_resamp <- 100

  method3_pred_int <-
    unsup_repeated_subsample(Y = Y, alpha = alpha, k_val = k_val,
                             n_resamp = n_resamp)

  # Store method 3 results
  pred_ints[sim == i, method_3_lb := method3_pred_int$lower_bound]
  pred_ints[sim == i, method_3_ub := method3_pred_int$upper_bound]

}

pred_ints
pred_ints

# Min and max values for each bound
summary(pred_ints$method_2_lb)
summary(pred_ints$method_2_ub)

summary(pred_ints$method_3_lb)
summary(pred_ints$method_3_ub)

# Save results
fwrite(pred_ints,
       file = "data/unsupervised/method_2/method_2_3_bounds.csv")
