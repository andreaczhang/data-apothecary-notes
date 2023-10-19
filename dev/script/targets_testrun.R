# this is my own test run script for targets package 
# install.packages('targets')

# run independently ----
library(readr)
library(dplyr)
library(ggplot2)

read_csv('data.csv', col_types = cols()) |> 
  filter(!is.na(Ozone))

d <- get_data('data.csv')
m <- fit_model(data = d)
m

plot_model(model = m, data = d)


# use targets ----
library(targets)

# this creates a _targets.R file in your root dir
# also two run files 
use_targets()

# this needs to stay out of _targets.R
# list the tasks 
tar_manifest()

tar_visnetwork()

# run pipeline defined in _targets.R
tar_make()
# this creates a folder _targets/ with a few sub-folders

tar_read(plot)
tar_read(model)

# would be nice to learn how to plan for multiple study plans
