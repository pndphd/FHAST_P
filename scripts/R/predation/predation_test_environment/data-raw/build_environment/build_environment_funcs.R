# creates one row of the table of cell coordinates
create_row <- function(row_num, params){
  row <- tibble::tibble(x = seq(0,
                        params$length,
                        params$interval
                        ), 
                y = row_num
                )
  return(row)
}

# creates a table of cell coordinates; each x,y is the lower left corner of the cell
create_cells <- function(params){
  env <- purrr::map_dfr(
    .x = seq(0,
             params$width - params$interval,
             params$interval
             ),
    .f = ~ create_row(., params)
  )
  return(env)
}

# adds environmental parameters to cells along the shore
add_shore_params <- function(cells, params){
  shore <- select_shore(cells, params) # subsets the shore cells
  complete_shore <- shore %>%
    dplyr::mutate(wood = add_env_data(., 
                               params$veg_wood_density_bins, 
                               prob = params$wood_prob
                               ),
           veg = add_env_data(., 
                              params$veg_wood_density_bins, 
                              prob = params$veg_prob
                              ),
           open = 1 - (wood + veg),
           depth = add_env_data(., 
                                params$depth, 
                                prob = params$shore_d_probs
                                ),
           velocity = add_env_data(., 
                                   params$velocity, 
                                   prob = params$shore_v_probs
                                   ),
           substrate = add_env_data(., 
                                    params$substrate, 
                                    prob = params$substrate_probs
                                    )
           )
  return(complete_shore)
}

# adds environmental parameters to cells in the channel
add_channel_params <- function(cells, params){
  channel <- select_channel(cells, params) # subsets the channel cells
  channel <- channel %>% 
    dplyr::mutate(wood = 0,
           veg = 0,
           open = 1,
           depth = add_env_data(., 
                                params$depth, 
                                prob = params$channel_d_probs
                                ),
           velocity = add_env_data(., 
                                   params$velocity, 
                                   prob = params$channel_v_probs
                                   ),
           substrate = add_env_data(., 
                                    params$substrate, 
                                    prob = params$substrate_probs
                                    )
           )
  return(channel)
}

#### Helpers

# adds a given type of environmental data to a cell based on initial parameters and probabilities of occurence
add_env_data <- function(cells, bins, probs){
  sample(bins, size = nrow(cells), prob = probs, replace=TRUE)
}  

# subsets the cells for the shore area
select_shore <- function(cells, params){
  cells %>% 
    dplyr::filter(!(y > 0 & y < params$width - params$interval))
}

# subsets the cells for the channel area
select_channel <- function(cells, params){
  cells %>% 
    dplyr::filter((y > 0 & y < params$width - params$interval))
}

# calculates total cover
total_cover <- function(wood, veg, open){
  cover <- wood + veg + 0.1 * open
}

cell_area <- function(params){
  params$interval^2
}