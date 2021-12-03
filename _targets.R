
library(targets)

tar_option_set(packages = c(
  "tidyverse",
  "ggplot2",
  "yaml",
  "cowplot"
))

# Phase target makefiles
source("0_process.R")
source("1_plot.R")

# Combined list of target outputs
c(p0_targets, p1_targets)


