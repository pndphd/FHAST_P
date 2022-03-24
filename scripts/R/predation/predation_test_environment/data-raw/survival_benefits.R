
# 0. load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(tictoc)
options(readr.show_col_types = FALSE)

# 1. load source scripts -----------------------------------------------------

# name of source folder
source_folder <- here("data-raw", "survival_benefits")

# list of script files
scripts <- list.files(source_folder, pattern = "*.R", recursive = TRUE)

# complete paths to all scripts
paths <- here(source_folder, scripts)

# load all scripts
walk(paths, source)

# 2. load data ---------------------------------------------------------------

# name of source folder
data_folder <- here("data-raw", "survival_benefits")

# list of script files
csv_files <- list.files(source_folder, pattern = "*.csv", recursive = TRUE)

# complete paths to all scripts
data_paths <- here(data_folder, csv_files)

# create the dataframe of raw data
survival_ben_data <- full_raw_data(data_paths, survival_benefit_params)

# 3. process data ------------------------------------------------------------

# fit each variable to a logistic model
model_table <- table_of_logistic_models(survival_ben_data)

# create a dataframe of with a range of values to make predictions over
x_vals <- table_of_x_vals(survival_benefit_params)

# make predictions and create a lookup table
survival_benefit_predictions <- survival_benefit_table(x_vals, model_table)

write_csv(survival_benefit_predictions, here('data', 'survival_benefits.csv'))
