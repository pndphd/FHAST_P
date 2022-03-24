
# Description -------------------------------------------------------------

# Author: Ted Hermann
# Date: 4 Mar. 2022
# Description: This script adds a predator column to the environment
# csv. It also adds distance to cover and the associated survival benefits 
# during a predation encounter. The latter may be removed at a later date.

# 0. load packages ---------------------------------------------------------

library(tidyverse)
library(here)
library(tools)
options(readr.show_col_types = FALSE)

# 1. load scripts ------------------------------------------------------------

source_folder <- here("R")
script_folders <- here(list.dirs(source_folder, recursive = FALSE))
scripts <- here(list.files(script_folders, full.names = TRUE))
walk(scripts, source)

# 2. load data ---------------------------------------------------------------

data_paths <- here("data", list.files(here("data")))
data <- map(data_paths, read_csv)

# giving sensible names to the dataframes in the list so they can be more easily selected
names(data) <- file_path_sans_ext(basename(data_paths))

# 3. add predators -----------------------------------------------------------

environment <- data$test_environment

# initial predator numbers is just the total divided by the number of cells
preds <- init_preds(environment, pred_params)

# calculate the total modifier value for each cell; influences how many preds in that cell
mods <- total_pred_modifiers(environment, pred_params)

# use pred modifiers to change the numbers of predators per cell
total_preds <- tibble(preds * mods) %>%
  mutate(preds = ifelse(preds < 0, 0, preds))

# calculate the total area occupied by predators per cell
pred_area_in_cells <- pred_area_in_cell(total_preds, pred_params)

# add the predator data to the environment
environment <- environment %>%
  bind_cols(pred_area_in_cells)

# use the cell area and pred area to calculate the probability of a pred encounter
environment <- environment %>%
  mutate(enc_prob = prob_of_pred_encounter(preds, cell_area))

# 4. model dis vs pct cover relationship -------------------------------------

model <- lm(mean_dis_w_0 ~ pct_cover * sqrt(pct_cover), data = data$cover_simulation_data)

# 5. add distance to cover values to environment ----------------------------

environment <- environment %>%
  mutate(
    dis_to_cover = predict(model, newdata = .),
    dis_to_cover = ifelse(dis_to_cover < 0, 0, dis_to_cover)
  )

# 6. add survival bonuses and probabilities to environment -------------------

survival_benefits <- data$survival_benefits

cover_survival_ben <- survival_benefits %>%
  filter(variable == "cover")

environment <- environment %>%
  group_nest(dis_to_cover) %>%
  mutate(
    surv_ben = map_dbl(round(dis_to_cover, 3), get_cover_survival_bonus, bonus_table = cover_survival_ben),
    survival_prob = map_dbl(surv_ben, get_survival_prob, pred_success = survival_params$pred_success)
  ) %>%
  unnest(data)