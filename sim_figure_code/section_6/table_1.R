# Values for table with coverage from sleep example

library(tidyverse)
library(data.table)

# Read in data
method_1 <- fread(file = "sim_data/section_6/method_1_coverage.csv") %>%
  mutate(Method = "CDF Pooling")

method_2 <- fread(file = "sim_data/section_6/method_2_coverage.csv") %>%
  mutate(Method = "Subsample Once")

method_3 <- fread(file = "sim_data/section_6/method_3_coverage.csv") %>%
  mutate(Method = "Subsample Avg")

##############################
##### Coverage estimates #####
##############################

# Method 1 coverage (avg, 2.5 quantile, 97.5 quantile)

method_1 %>%
  group_by(alpha) %>%
  dplyr::summarise(avg_coverage = mean(coverage),
                   q_025 = quantile(coverage, 0.025),
                   q_975 = quantile(coverage, 0.975)) %>%
  round(2)

# Method 2 coverage (avg, 2.5 quantile, 97.5 quantile)

method_2 %>%
  group_by(alpha) %>%
  dplyr::summarise(avg_coverage = mean(coverage),
                   q_025 = quantile(coverage, 0.025),
                   q_975 = quantile(coverage, 0.975)) %>%
  round(2)

# Method 3 coverage (avg, 2.5 quantile, 97.5 quantile)

method_3 %>%
  group_by(alpha) %>%
  dplyr::summarise(avg_coverage = mean(coverage),
                   q_025 = quantile(coverage, 0.025),
                   q_975 = quantile(coverage, 0.975)) %>%
  round(2)
