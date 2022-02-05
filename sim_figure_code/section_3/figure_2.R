# Coverage/size plots for unsupervised methods on unbalanced data

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

method_1 <- fread(file = "sim_data/section_3/unbalanced/method_1.csv") %>%
  mutate(Method = "1. Pool CDFs")

method_2 <- fread(file = "sim_data/section_3/unbalanced/method_2.csv") %>%
  mutate(Method = "2. Subsample Once")

method_3 <- fread(file = "sim_data/section_3/unbalanced/method_3.csv") %>%
  mutate(Method = "3. Repeated Subsample")

# Merge results across methods
results <- rbind(method_1, method_2, method_3) %>%
  mutate(Method = factor(Method,
                         levels = c("1. Pool CDFs",
                                    "2. Subsample Once",
                                    "3. Repeated Subsample"),
                         labels = c("1. Pool CDFs",
                                    "2. Subsample Once",
                                    "3. Repeated Subsample")))

########################
##### Create plots #####
########################

# Plot coverage
coverage_unbal <- results %>%
  ggplot(aes(x = k, y = coverage, col = Method)) +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0.9, lty = "dashed") +
  scale_y_continuous(limits = c(0.65, 1)) +
  labs(x = "Number of Groups (k)",
       y = "Coverage",
       title = "Coverage by Method") +
  scale_color_manual(values = c("#FF3636", "black", "#2059FF"),
                     labels = c("1. Pool CDFs",
                                "2. Subsample Once",
                                "3. Repeated Subsample")) +
  paper_theme +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12))

# Plot size
size_unbal <- results %>%
  ggplot(aes(x = k, y = avg_length, col = Method)) +
  geom_point() +
  geom_line() +
  labs(x = "Number of Groups (k)",
       y = "Average Prediction Interval Length",
       title = "Interval Length by Method") +
  scale_color_manual(values = c("#FF3636", "black", "#2059FF"),
                     labels = c("1. Pool CDFs",
                                "2. Subsample Once",
                                "3. Repeated Subsample")) +
  paper_theme +
  theme(legend.position = "none",
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12))

# Code to extract legend: http://www.sthda.com/english/wiki/wiki.php?id_contents=7930#add-a-common-legend-for-multiple-ggplot2-graphs
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(coverage_unbal)

coverage_unbal <- coverage_unbal + theme(legend.position = "none")

# Combine plots of coverage and pred int length in unbalanced setting

unbal_coverage_size <-
  grid.arrange(coverage_unbal, size_unbal, legend, ncol = 2, nrow = 2,
               layout_matrix = rbind(c(1, 2), c(3, 3)),
               widths = c(2.7, 2.7), heights = c(2, 0.2))

######################
##### Save plots #####
######################

ggsave(plot = unbal_coverage_size,
       filename = "sim_figures/section_3/unbal_coverage_size.pdf",
       width = 10, height = 3.5)
