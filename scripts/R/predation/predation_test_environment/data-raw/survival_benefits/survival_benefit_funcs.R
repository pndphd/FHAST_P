# processes length-frequency data of predators

process_length_data <- function(path){
  data <- read_csv(path)   # read in the data
  data <- data %>%
    mutate(max_prey_length = prey_conv(4.64, 2.48e-6, length_mm), # sets max prey length per predator size
           safety = cumulative_proportion - proportion_of_total, # calculates "safety" as the proportion of preds too small to pose a risk
           angle = angle_calc(length_mm), # calculates an angle used for reaction distance
           reaction_distance = max_prey_length / (2 * tan(angle / 2)), # reaction distance of predators at a given size
           max_prey_length = max_prey_length / 10, # max size of prey coverted to cm
           variable = "length") %>% # creates a variable "length" for sorting later
    dplyr::rename(magnitude = max_prey_length, unitless_value = safety) %>% # renames values for better sorting
    dplyr::select(variable, magnitude, unitless_value) %>% # selects the necessary values for modeling
    dplyr::filter(magnitude<=30) # only wanted prey size under 30 cm; not really releveant outside this range
  return(data)
}

# processes data on the concentration of salmonids under various cover conditions

process_cover_data <- function(path){
  max_survival <- 0.9
  data <- read_csv(path)  # read in the data
  data <- data %>%
    filter(variable == "Dis to Cover") %>%
    group_by(fishSize_mm) %>% # group by fish size
    mutate(fraction = cumlitaveFraction - lag(cumlitaveFraction),
           fraction = ifelse(is.na(fraction), cumlitaveFraction, fraction),
           unitless_value = fraction/max(fraction, na.rm = T)*max_survival) %>%
    ungroup() %>% 
    dplyr::rename(magnitude = value) %>% 
    dplyr::select(variable, magnitude, unitless_value) # selects the necessary values for modeling
  return(data)
}

# processes data on predator activity and digestion vs temperature

process_temp_data <- function(path){
  data <- read_csv(path)# read in the data
  data <- data %>% 
    group_by(author, year, journal, species, experiment) %>% # group by various factors so only data from the same experiments are changed
    mutate(unitless_value = y/max(y), # convert to values relative to the max of each experiment
           variable = 'temperature') %>% # add a variable "temperature" for later sorting
    ungroup() %>% 
    dplyr::rename(magnitude = x) %>% 
    dplyr::select(variable, magnitude, unitless_value) # selects the necessary values for modeling
  return(data)
}

# runs all the data functions and combines them into a single dataframe
# uses the "dummy" function to map over multiple functions

full_raw_data <- function(paths, params){
  df <- bind_rows(map2(.x = paths, .y = params$funcs, .f = dummy))
  return(df)
}

# create a table of values for each variable over the range and increment specified above 

table_of_x_vals <- function(params){
  df <- map2_dfr(params$variables, params$var_ranges, make_tib)
  return(df)
}

# creates a table of logistic models for each parameter

table_of_logistic_models <- function(df){
  df %>%
    # nest the table per variable
    nest(data = c(magnitude, unitless_value)) %>%
    
    # add a new column of fitted glm models for each variable
    mutate(fit = purrr::map(data, ~ glm(unitless_value ~ magnitude,
                                        family = quasibinomial(logit),
                                        data = .)))
}

# outputs a table of predicted survival values for each variable affecting predation

survival_benefit_table <- function(x_val_table, model_table){

  predict_vals <- x_val_table %>%
    # nest the table per variable
    group_nest(variable) %>%
    
    # preditct survival values based on the x values and glms
    mutate(survival = purrr::map2(model_table$fit, data, predict.glm, type='response')) %>%
    
    # unnest the x values and survival values
    unnest(everything()) %>%
    
    # save the variable name, x value, and survival prediction
    dplyr::select(variable, magnitude, survival)
  return(predict_vals)
}

#### Helpers

# function to calculate gape-limited prey size for a given predator size (based on species)

prey_conv <- function(a, B, pred_L){
  exp(a + B * pred_L^2)
}

# an angle value used to calculate the reaction distance of predators

angle_calc <- function(length){
  0.0167 * exp(9.14 - 2.4 * log(length) + 0.229 * log(length)^2)
}

# function to calculate cumulative percent
cumpct <- function(x){
  cumpct = cumsum(x) / sum(x)
  return(cumpct)
}

# dummy function that allows map to access lists of functions
dummy <- function(path, func){
  func(path)
}

# makes a simple tibble with a variable as column 1 and a range of values as column 2
make_tib <- function(variable, sequence){
  tib <- tibble(variable = variable, magnitude = sequence)
  return(tib)
}


