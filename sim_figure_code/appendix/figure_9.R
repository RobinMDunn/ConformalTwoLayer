# Coverage/size plots for unsupervised method on theta_j ~ Exp(1)
# and Y_{ji} ~ Beta(theta_j, 1)

library(grid)
library(gridExtra)
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
method_0 <- fread(file = "sim_data/appendix/unsup_exp_beta/method_0.csv") %>%
  mutate(Method = "0. Double Conformal")

method_1 <- fread(file = "sim_data/appendix/unsup_exp_beta/method_1.csv") %>%
  mutate(Method = "1. Pool CDFs")

method_2 <- fread(file = "sim_data/appendix/unsup_exp_beta/method_2.csv") %>%
  mutate(Method = "2. Subsample Once")

method_3 <- fread(file = "sim_data/appendix/unsup_exp_beta/method_3.csv") %>%
  rename(coverage = coverage_2alpha, avg_length = avg_length_2alpha) %>%
  mutate(Method = "3. Repeated Subsample")

# Merge results across methods
results <- rbind(method_0, method_1, method_2, method_3)

########################
##### Create plots #####
########################

# Coverage vs k, smaller values of k
cov_small_k <- results %>%
  dplyr::filter(k <= 100, n == 100, !is.na(coverage)) %>%
  mutate(Method = factor(
    Method,
    levels = c("0. Double Conformal",
               "1. Pool CDFs",
               "2. Subsample Once",
               "3. Repeated Subsample"),
    labels = c("0. Double Conformal",
               "1. Pool CDFs",
               "2. Subsample Once",
               "3. Repeated Subsample"))) %>%
  ggplot(aes(x = k, y = coverage, color = Method)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  geom_hline(yintercept = 0.90, lty = "dashed") +
  lims(y = c(0.8, 1)) +
  labs(x = "Number of Groups (k)",
       y = "Coverage",
       title = "Coverage by Method") +
  scale_color_manual(values = c("#AB62F4", "#FF3636", "black", "#2059FF"),
                     labels = c("0. Double Conformal",
                                "1. Pool CDFs",
                                "2. Subsample Once",
                                "3. Repeated Subsample")) +
  paper_theme +
  theme(legend.position = "bottom")

# Size vs k, smaller values of k
size_small_k <- results %>%
  dplyr::filter(k <= 100, n == 100, !is.na(coverage)) %>%
  mutate(Method = factor(
    Method,
    levels = c("0. Double Conformal",
               "1. Pool CDFs",
               "2. Subsample Once",
               "3. Repeated Subsample"),
    labels = c("0. Double Conformal",
               "1. Pool CDFs",
               "2. Subsample Once",
               "3. Repeated Subsample"))) %>%
  ggplot(aes(x = k, y = avg_length, color = Method)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  labs(x = "Number of Groups (k)",
       y = "Average Pred Int Length",
       title = "Interval Length by Method") +
  scale_color_manual(values = c("#AB62F4", "#FF3636", "black", "#2059FF"),
                     labels = c("0. Double Conformal",
                                "1. Pool CDFs",
                                "2. Subsample Once",
                                "3. Repeated Subsample")) +
  paper_theme +
  theme(legend.position = "none")

# Code to extract legend: http://www.sthda.com/english/wiki/wiki.php?id_contents=7930#add-a-common-legend-for-multiple-ggplot2-graphs
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(cov_small_k)

cov_small_k <- cov_small_k + theme(legend.position = "none")

# Combine plots of coverage and pred int length in unbalanced setting

exp_theta_beta_Y_coverage_size <-
  grid.arrange(cov_small_k, size_small_k, legend, ncol = 2, nrow = 2,
               layout_matrix = rbind(c(1, 2), c(3, 3)),
               widths = c(2.7, 2.7), heights = c(2, 0.2),
               top = textGrob(expression("Unsupervised Prediction."~theta[j]~
                                           "~ Exp(1),"~Y["ji"]~"~ Beta("*theta[j]*", 1)."),
                              gp = gpar(fontsize = 15,font = 8)))

######################
##### Save plots #####
######################

ggsave(plot = exp_theta_beta_Y_coverage_size,
       filename = "sim_figures/appendix/exp_theta_beta_Y.pdf",
       width = 10, height = 4)
