# Size plots for shrinkage methods

library(tidyverse)
library(data.table)

# Create theme
paper_theme <- theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.spacing = unit(1.2, "lines"))

# Read in data
method_1 <- fread(file = "sim_data/section_5/method_1.csv") %>%
  mutate(Method = "1. Group 1 Only")

method_2 <- fread(file = "sim_data/section_5/method_2.csv") %>%
  mutate(Method = "2. Shrinkage")

# Merge results across methods
results <- rbind(method_1, method_2)

########################
##### Create plots #####
########################

size_resid <- results %>%
  mutate(sigma_sq = factor(sigma_sq, levels = c(1, 100),
                           labels = c("sigma^2==1", "sigma^2==100"))) %>%
  ggplot(aes(x = k, y = avg_length, col = Method)) +
  facet_wrap(. ~ sigma_sq, scale = "free_y", labeller = label_parsed) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", formula = y ~ x, se = F) +
  scale_color_manual(values = c("red", "blue")) +
  labs(y = "Mean Size of Conformal Sets",
       x = "Number of Groups") +
  paper_theme +
  theme(plot.title = NULL,
        plot.subtitle = NULL)

######################
##### Save plots #####
######################

ggsave(plot = size_resid,
       filename = "sim_figures/section_5/shrinkage_size.pdf",
       width = 8.5, height = 3)
