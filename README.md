# ConformalTwoLayer Repository Overview

This repository contains code to replicate the results of [Distribution-free prediction sets for two-layer hierarchical models](https://arxiv.org/abs/1809.07441) by [Robin Dunn](https://robinmdunn.github.io/), [Larry Wasserman](https://www.stat.cmu.edu/~larry/), and [Aaditya Ramdas](http://www.stat.cmu.edu/~aramdas/). The `ConformalTwoLayer` library contains functions to reproduce the simulations.

## Folder contents

- [R](R): Function definitions
- [man](man): Package documentation files
- [sim_code](sim_code): Code for the paper's simulations. The subfolders [section_3](sim_code/section_3), [section_4](sim_code/section_4), [section_5](sim_code/section_5), [section_6](sim_code/section_6), and [appendix](sim_code/appendix) contain code to reproduce the simulations for each respective section. ([sim_data](sim_data), [sim_figure_code](sim_figure_code), and [sim_figures](sim_figures) have similar subfolder structures.) Each R script saves the simulation output to [sim_data](sim_data).
- [sim_data](sim_data): Output of simulations from [sim_code](sim_code).
- [sim_figure_code](sim_figure_code): Code to reproduce the paper's plots and tables. Reads in data from [sim_data](sim_data) and outputs plots to [sim_figures](sim_figures).
- [sim_figures](sim_figures): Plots from the paper. The plots are the output of the scripts in [sim_figure_code](sim_figure_code).

## Installing the ConformalTwoLayer package

We can use `devtools` to install the `ConformalTwoLayer` package.

```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load ConformalTwoLayer
devtools::install_github("RobinMDunn/ConformalTwoLayer")
library(ConformalTwoLayer)
```

## Run the simulations for a given section

The R scripts in [sim_code](sim_code) run the simulations and output the simulated data to [sim_data](sim_data). To run the simulations for a given section, find the folder corresponding to the simulation's section: [section_3](sim_code/section_3), [section_4](sim_code/section_4), [section_5](sim_code/section_5), [section_6](sim_code/section_6), or [appendix](sim_code/appendix). If you have cloned the `ConformalTwoLayer` repository, all scripts can be run from the `ConformalTwoLayer` directory with no additional input. Users may also use the optional arguments at the top of the scripts to run a subset of the simulations.

## Reproduce a figure without rerunning the simulations

The R scripts in [sim_figure_code](sim_figure_code) read in the necessary simulated data from the [sim_data](sim_data) folder and output the figures to the [sim_figures](sim_figures) folder. To reproduce a given figure, within [sim_figure_code](sim_figure_code) find the folder corresponding to the figure's section: [section_3](sim_figure_code/section_3), [section_4](sim_figure_code/section_4), [section_5](sim_figure_code/section_5), [section_6](sim_figure_code/section_6), or [appendix](sim_figure_code/appendix). Within these subfolders, the name of each R script corresponds to the figure that the script produces.
