# Size plots for sleep example

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
method_1 <- fread(file = "sim_data/section_6/method_1_size.csv") %>%
  dplyr::mutate(Method = "1. Pool CDFs")

method_2 <- fread(file = "sim_data/section_6/method_2_size.csv") %>%
  dplyr::mutate(Method = "2. Subsample Once")

method_3 <- fread(file = "sim_data/section_6/method_3_size.csv") %>%
  dplyr::mutate(Method = "3. Repeated Subsample")

# Merge results across methods
results <- rbind(method_1, method_2, method_3) %>%
  dplyr::mutate(
    Method = factor(Method,
                    levels = c("1. Pool CDFs",
                               "2. Subsample Once",
                               "3. Repeated Subsample"),
                    labels = c("1. Pool CDFs",
                               "2. Subsample Once",
                               "3. Repeated Subsample")),
    PI_length = max_pred_int - min_pred_int,
    Days = factor(Days,
                  levels = c(1, 5, 9),
                  labels = c("1 Day", "5 Days", "9 Days")))

########################
##### Create plots #####
########################

# Plot conformal intervals for a subset of day 0 reaction times
pred_int_plot <- results %>%
  dplyr::filter(Baseline %in% seq(200, 320, by = 30)) %>%
  dplyr::mutate(Baseline = factor(Baseline)) %>%
  ggplot() +
  facet_grid(Days ~ .) +
  geom_linerange(aes(x = Baseline,
                     ymin = min_pred_int, ymax = max_pred_int, color = Method),
                 position = position_dodge(0.5)) +
  labs(x = "Day 0 Reaction Time",
       y = "Predicted Reaction Time",
       title = "Conformal Intervals") +
  scale_color_manual(values = c("#FF6363", "black", "#2059FF"),
                     labels = c("1. Pool CDFs", "2. Subsample Once",
                                "3. Repeated Subsample")) +
  paper_theme +
  theme(legend.position = "none",
        plot.title = element_text(size = 14))

# Length of conformal intervals
pred_int_length <- results %>%
  ggplot(aes(x = Baseline, y = PI_length, col = Method)) +
  facet_grid(Days ~ .) +
  geom_point() +
  geom_line() +
  labs(x = "Day 0 Reaction Time",
       y = "Interval Length",
       title = "Conformal Interval Length") +
  scale_color_manual(values = c("#FF6363", "black", "#2059FF"),
                     labels = c("1. Pool CDFs", "2. Subsample Once",
                                "3. Repeated Subsample")) +
  paper_theme +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14))

# Code to extract legend: http://www.sthda.com/english/wiki/wiki.php?id_contents=7930#add-a-common-legend-for-multiple-ggplot2-graphs
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(pred_int_length)

pred_int_length <- pred_int_length + theme(legend.position = "none")

# Combine plots of intervals and length of intervals
sleep_pi_length <-
  grid.arrange(pred_int_plot, pred_int_length, legend, ncol=2, nrow = 2,
               layout_matrix = rbind(c(1, 2), c(3, 3)),
               widths = c(2.7, 2.7), heights = c(2.5, 0.2),
               top = textGrob(paste("Reaction Time given Baseline Time and",
                                    "1/5/9 Days of Sleep Deprivation"),
                              gp = gpar(fontsize = 15,font = 8)))

######################
##### Save plots #####
######################

ggsave(plot = sleep_pi_length,
       filename = "sim_figures/section_6/sleep_pi_length.pdf",
       width = 9.5, height = 7)
