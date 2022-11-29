
library(targets)

tar_option_set(packages = c(
  "tidyverse",
  "ggplot2",
  "yaml",
  "cowplot",
  "MuMIn",
  "imputeTS"
))

# Phase target makefiles
source("0_process.R")
source("1_plot.R")
source("2_stats.R")
source("3_data_release.R")

# Combined list of target outputs
c(p0_targets,
  p1_targets,
  p2_targets,
  p3_targets)


