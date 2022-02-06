# Coverage/size plots for supervised methods

library(tidyverse)
library(data.table)
library(gridExtra)
library(grid)

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
method_1 <- fread(file = "sim_data/section_4/method_1.csv") %>%
  dplyr::mutate(Method = "1. Pool CDFs")

method_2 <- fread(file = "sim_data/section_4/method_2.csv") %>%
  dplyr::mutate(Method = "2. Subsample Once")

method_3 <- fread(file = "sim_data/section_4/method_3.csv") %>%
  dplyr::rename(coverage = coverage_2alpha, avg_size = avg_size_2alpha) %>%
  dplyr::mutate(Method = "3. Repeated Subsample")

# Merge results across methods
results <- rbind(method_1, method_2, method_3, fill = TRUE)

# Code to extract legend: http://www.sthda.com/english/wiki/wiki.php?id_contents=7930#add-a-common-legend-for-multiple-ggplot2-graphs
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

########################
##### Create plots #####
########################

##### Coverage #####

# Coverage vs k, smaller values of k
single_cov_small_k <- results %>%
  dplyr::filter(mu == 0, tau_sq == 1, n == 100, k <= 100) %>%
  ggplot(aes(x = k, y = coverage, color = Method)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0.90, lty = "dashed") +
  lims(y = c(0.86, 1)) +
  labs(x = "Number of Groups (k)",
       y = "Coverage",
       title = "Smaller k Values") +
  scale_color_manual(values = c("#FF3636", "black", "#2059FF")) +
  paper_theme +
  theme(legend.position = "bottom")

# Coverage vs k, larger values of k
single_cov_large_k <- results %>%
  dplyr::filter(mu == 0, tau_sq == 1, n == 100, k >= 200) %>%
  ggplot(aes(x = k, y = coverage, color = Method)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0.90, lty = "dashed") +
  lims(y = c(0.86, 1)) +
  labs(x = "Number of Groups (k)",
       y = "Coverage",
       title = "Larger k Values") +
  scale_color_manual(values = c("#FF3636", "black", "#2059FF")) +
  paper_theme +
  theme(legend.position = "none")

# Combined coverage plot
legend <- get_legend(single_cov_small_k)

single_cov_small_k <- single_cov_small_k + theme(legend.position = "none")

cov_n100 <-
  grid.arrange(single_cov_small_k, single_cov_large_k, legend, ncol = 2, nrow = 2,
               layout_matrix = rbind(c(1, 2), c(3, 3)),
               widths = c(2.7, 2.7), heights = c(2, 0.2))

##### Size #####

# Size vs k, smaller values of k
single_size_small_k <- results %>%
  dplyr::filter(mu == 0, tau_sq == 1, n == 100, k <= 100) %>%
  ggplot(aes(x = k, y = avg_size, color = Method)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  labs(x = "Number of Groups (k)",
       y = "Average Prediction Set Size",
       title = "Smaller k Values") +
  scale_color_manual(values = c("#FF3636", "black", "#2059FF")) +
  paper_theme +
  theme(legend.position = "bottom")

# Size vs k, larger values of k
single_size_large_k <- results %>%
  dplyr::filter(mu == 0, tau_sq == 1, n == 100, k >= 200) %>%
  ggplot(aes(x = k, y = avg_size, color = Method)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  labs(x = "Number of Groups (k)",
       y = "Average Prediction Set Size",
       title = "Larger k Values") +
  scale_color_manual(values = c("#FF3636", "black", "#2059FF")) +
  paper_theme +
  theme(legend.position = "none")

# Combined coverage plot
legend <- get_legend(single_size_small_k)

single_size_small_k <- single_size_small_k + theme(legend.position = "none")

size_n100 <-
  grid.arrange(single_size_small_k, single_size_large_k, legend, ncol = 2, nrow = 2,
               layout_matrix = rbind(c(1, 2), c(3, 3)),
               widths = c(2.7, 2.7), heights = c(2, 0.2))

######################
##### Save plots #####
######################

ggsave(plot = cov_n100,
       filename = "sim_figures/section_4/sup_cov_n100.pdf",
       width = 10, height = 3.5)

ggsave(plot = size_n100,
       filename = "sim_figures/section_4/sup_size_n100.pdf",
       width = 10, height = 3.5)
