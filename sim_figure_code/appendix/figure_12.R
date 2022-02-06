# Size plots for supervised methods on normal data
# across additional n_j

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
method_1 <- fread(file = "sim_data/appendix/sup_addl/method_1.csv") %>%
  dplyr::mutate(Method = "1. Pool CDFs")

method_2 <- fread(file = "sim_data/appendix/sup_addl/method_2.csv") %>%
  dplyr::mutate(Method = "2. Subsample Once")

method_3 <- fread(file = "sim_data/appendix/sup_addl/method_3.csv") %>%
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

##### Size #####

# Size vs k, smaller values of k
size_small_k <- results %>%
  dplyr::filter(k <= 100, n %in% c(20, 100, 1000)) %>%
  dplyr::mutate(Method = factor(
    Method,
    levels = c("1. Pool CDFs",
               "2. Subsample Once",
               "3. Repeated Subsample"),
    labels = c("1. Pool CDFs",
               "2. Subsample Once",
               "3. Repeated Subsample"))) %>%
  dplyr::mutate(n = factor(n, levels = c(20, 100, 1000),
                    labels = c("n[j]==20~obs~per~group",
                               "n[j]==100~obs~per~group",
                               "n[j]==1000~obs~per~group")),
         Params = factor(mu, levels = c(0, 1),
                         labels = expression(atop(mu~"= 0,", tau^2~"= 1"),
                                             atop(mu~"= 1,", tau^2~"= 0.1")))) %>%
  ggplot(aes(x = k, y = avg_size, color = Method)) +
  facet_grid(Params ~ n, labeller = label_parsed, scale = "free_y") +
  geom_point(alpha = 0.5) +
  geom_line() +
  labs(x = "Number of Groups (k)",
       y = "Average Prediction Set Size",
       title = expression("Supervised Prediction. Set Size by Method."~
                            "Smaller k Values."~theta[1]*","*ldots*","*theta[k]~
                            "~ N("*mu*","~tau^2*")")) +
  scale_color_manual(values = c("#FF3636", "black", "#2059FF")) +
  paper_theme +
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 0))

# Size vs k, larger values of k
size_large_k <- results %>%
  dplyr::filter(k >= 200, n %in% c(20, 100, 1000)) %>%
  dplyr::mutate(Method = factor(
    Method,
    levels = c("1. Pool CDFs",
               "2. Subsample Once",
               "3. Repeated Subsample"),
    labels = c("1. Pool CDFs",
               "2. Subsample Once",
               "3. Repeated Subsample"))) %>%
  dplyr::mutate(n = factor(n, levels = c(20, 100, 1000),
                    labels = c("n[j]==20~obs~per~group",
                               "n[j]==100~obs~per~group",
                               "n[j]==1000~obs~per~group")),
         Params = factor(mu, levels = c(0, 1),
                         labels = expression(atop(mu~"= 0,", tau^2~"= 1"),
                                             atop(mu~"= 1,", tau^2~"= 0.1")))) %>%
  ggplot(aes(x = k, y = avg_size, color = Method)) +
  facet_grid(Params ~ n, labeller = label_parsed, scale = "free_y") +
  geom_point(alpha = 0.5) +
  geom_line() +
  labs(x = "Number of Groups (k)",
       y = "Average Prediction Set Size",
       title = expression("Supervised Prediction. Set Size by Method."~
                            "Larger k Values."~theta[1]*","*ldots*","*theta[k]~
                            "~ N("*mu*","~tau^2*")")) +
  scale_color_manual(values = c("#FF3636", "black", "#2059FF"))  +
  paper_theme +
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 0))

######################
##### Save plots #####
######################

ggsave(plot = size_small_k,
       filename = "sim_figures/appendix/sup_size_small_k.pdf",
       width = 8.5, height = 4.5)

ggsave(plot = size_large_k,
       filename = "sim_figures/appendix/sup_size_large_k.pdf",
       width = 8.5, height = 4.5)
