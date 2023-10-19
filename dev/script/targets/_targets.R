library(targets)
source("R/functions.R")
# packages needed
tar_option_set(packages = c("readr", "dplyr", "ggplot2"))

# tasks (declare a target)
# name (object), command
# a list of 4
list(
  tar_target(file, "data.csv", format = "file"),
  tar_target(data, get_data(file)),
  tar_target(model, fit_model(data)),
  tar_target(plot, plot_model(model, data))
)



