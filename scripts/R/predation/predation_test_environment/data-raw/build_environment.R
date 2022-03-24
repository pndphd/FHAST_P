
# 0. load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(tictoc)
options(readr.show_col_types = FALSE)

# 1. load source scripts -----------------------------------------------------

# name of source folder
source_folder <- here("data-raw", "build_environment")

# list of script files
scripts <- list.files(source_folder, recursive = TRUE)

# complete paths to all scripts
paths <- here(source_folder, scripts)

# load all scripts
walk(paths, source)


# 2. build environment -------------------------------------------------------


# create empty cells
cells <- create_cells(build_environment_params)

# add environmental parameters to the cells that represent shoreline
shore <- add_shore_params(cells, build_environment_params)

# add environmental parameters to the cells that represent the channel
channel <- add_channel_params(cells, build_environment_params)

# combine shore and channel and add values for total percent cover and cell area
river_reach <-  dplyr::bind_rows(shore, channel) %>%
  dplyr::arrange(y) %>%
  dplyr::mutate(pct_cover = total_cover(wood, veg, open),
                cell_area = cell_area(build_environment_params))

write_csv(river_reach, here('data', 'test_environment.csv'))
