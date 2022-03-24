
# 0. load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(tictoc)
options(readr.show_col_types = FALSE)

# 1. load source scripts -----------------------------------------------------

# name of source folder
source_folder <- here("data-raw", "cover_simulation")

# list of script files
scripts <- list.files(source_folder, recursive = TRUE)

# complete paths to all scripts
paths <- here(source_folder, scripts)

# load all scripts
walk(paths, source)

# 2. run simulation ----------------------------------------------------------

# path to the cover simulation data
cover_sim_path <- here('data', 'cover_simulation_data.csv')

# the simulation takes a couple minutes, so saving the data is helpful
if(!file.exists(cover_sim_path)){
  write_csv(get_cover_vs_dis(cover_sim_params), cover_sim_path)
}
