# creates a vector of evenly distributed predators equal to the number of cells in the environment
init_preds <- function(environment, params){
  preds <- tibble::tibble(preds = rep(pred_avg(environment, params) / nrow(environment), 
                                      each = nrow(environment)
                                      )
                          )
  return(preds)
}

# selects the relevant predator modifers for a given environmental parameter
get_env_mod <- function(col_name, df, weights){
  weight_list <- weights[[col_name]] # select the weights
  data <- df[[col_name]] # subset the data
  env_mod <- purrr::map_dbl(data, 
                            look_up_value, 
                            weight_list = weight_list
  ) # check the data in the predator weight look up table
  return(env_mod)
}

# make a dataframe of predator modifiers per cell and per environmental parameter
env_modifiers <- function(environment, params){
  condition_weights <- params$condition_weights # select the weights for the parameters
  cw_list <- purrr::map(condition_weights, data.frame) # turn those weights into a list of dataframes for easier handling
  full_env_mods <- dplyr::bind_cols(
    setNames(
      purrr::map(
        .x = params$condition_types, 
        .f = get_env_mod, 
        df = environment, 
        weights = cw_list), 
      paste0(
        params$condition_types)
    )
  )
  return(full_env_mods)
}

# create the predator modifiers based on cover data, modified by the proportional area of each cover type in the cell
cover_modifiers <- function(environment, params){
  cover_data <- environment[params$cover_types]
  cover_mod <- purrr::map(.x = params$cover_types, 
                          .f = get_cover_mod, 
                          cover_weight = params$cover_weights, 
                          cover_data = cover_data
  )
  return(cover_mod)
}


# combine all predator modifiers into one multiplier; values are additive
total_pred_modifiers <- function(environment, params){
  cover_mods <- cover_modifiers(environment, params)
  env_mods <- env_modifiers(environment, params)
  full_df <- dplyr::bind_cols(cover_mods, env_mods)
  total_mods <- full_df %>% 
    dplyr::mutate(modifier = 1 + rowSums(.)) %>%
    dplyr::pull(modifier)
  return(total_mods)
}

# calculate the total predator value for a cell based on the modifiers and the initial predator average number
total_preds <- function(environment, params){
  preds <- init_preds(environment, params)
  mods <- total_pred_modifiers(environment, params)
  total <- tibble::tibble(preds * mods) %>% 
    dplyr::mutate(preds = ifelse(preds < 0, 0, preds))
  return(total)
}

# calculate the area occupied by predators in each cell
pred_area_in_cell <- function(preds, params){
  reaction_area <- pred_reaction_area(params)
  total_area <- preds * reaction_area
  return(total_area)
}

#### helpers

# calculates the average number of predators per cell
pred_avg <- function(environment, params){
  env_length <- environment %>% select(x) %>% max()
  params$preds_km * env_length / 1000
}

# finds the predator modifier "weight" corresponding to a specific value for an environmental parameter
look_up_value <- function(data, weight_list){
  weight_list[[data]]
}

# calculates the predator modifier for a specific cover type based on weight and proportional area covered
get_cover_mod <- function(cover_type, cover_weight, cover_data){
  cover_data[{{cover_type}}] * cover_weight[{{cover_type}}]
}

# calculates the area within which a predator reacts to prey
pred_reaction_area <- function(params){
  pi * params$pred_react_distance^2
}

prob_of_pred_encounter <- function(pred_area, cell_area){
  pred_area / cell_area
}
  
