# This function takes a shape file and a grid and samples a specified column form
# the shape file on the grid. It then returns the grid with a new column of the
# values  

##### sample_shape_with_grid #####
sample_shape_with_grid = function(grid, shape_file, column_name)
{
  # fix up the grid file
  grid = grid %>% 
    select(distance, area, left_or_right, geometry, lat_dist) %>% 
    rowid_to_column("ID")
  
  # This suppresses a warning about attributes
  st_agr(grid) = "constant"
  st_agr(shape_file) = "constant"
  
  # First intersect the shape file and grid and only save the unique IDs of each
  # resulting shape
  grid_samples = shape_file %>% 
    st_intersection(grid) %>% 
    mutate(sample_area = as.numeric(st_area(.))) %>% 
    as_tibble() %>% 
    select(sample_area, all_of(column_name), ID)  

  # Now take all those shapes and used a weighted average to get the average
  # (including areas without values as 0s) value over each grid cell
  new_grid = grid %>% 
    as_tibble() %>%
    select(ID, area) %>% 
    full_join(grid_samples, by = "ID") %>% 
    replace(is.na(.), 0) %>% 
    mutate(across(all_of(column_name), .fns = ~(. * sample_area), .names = "weights")) %>% 
    group_by(ID) %>% 
    summarize(value = sum(weights),
              area = mean(area)) %>% 
    ungroup() %>% 
    mutate(!!column_name := round(value/area,2)) %>% 
    select(all_of(column_name), ID) %>% 
    full_join(grid, by = "ID")

  return(new_grid)
}

##### sample_all_shapes #####
sample_all_shapes = function(grid = NULL,
                             shape_file = NULL,
                             column_name = NULL){
  
  output = future_map2(column_name, shape_files, ~sample_shape_with_grid(grid, .y, .x)) %>% 
    # Join the data frames together
    reduce(~left_join(.x, .y, by= c("ID", "distance", "area", "left_or_right", "geometry", "lat_dist"))) %>% 
    # Make it a shape file
    st_as_sf(sf_column_name = "geometry")
  
  return(output)
}


##### sampeled_to_csv #####
sampeled_to_csv = function(sampeled_shapes=NULL){
  shapes_csv = sampeled_shapes %>% 
  as_tibble() %>%
  mutate(lat_dist = ifelse(lat_dist>0,ifelse(left_or_right>0, lat_dist, -lat_dist),0)) %>% 
  select(Cover, distance, area, lat_dist, Zones, Value)
  return(shapes_csv)
}