# This file has the functions used in River_Grid_Maker_Run.R #

##### load_input_files #####
# This first file loads and conditions the input files 
load_input_files = function(line = NULL,
                            top = NULL,
                            EPGS = NULL){
  
  # Load the center line and smooth it
  river_line = st_read(line, quiet = TRUE) %>%
    st_transform(EPSG) %>%
    st_zm() %>%
    # bandwidth = resolution*10 was a good value in testing
    smooth(method = "ksmooth",
           max_distance = resolution,
           bandwidth = resolution*10)
  
  # Load the top of the reach marker
  top_marker = st_read(top, quiet = TRUE) %>%
    st_transform(EPSG) %>%
    st_zm()
  
  output = list(line = river_line,
                top = top_marker)
  return(output)
}

##### make_distances_list #####
# This Function makes the list of distances
make_distances_list = function(resolution = NULL,
                               buffer = NULL){
  distances = seq(resolution/2, max_buffer + resolution, resolution)
  return(distances)
}

##### make_buffers #####
make_buffers = function(distances = NULL,
                       line = NULL){
  
  buffers = map(distances, ~buffer_lines(line, .x)) %>% 
    do.call(rbind, .) %>% 
    st_difference() %>% 
    filter(st_is(., c("POLYGON","MULTIPOLYGON"))) 
  
  return(buffers)
}

##### buffer_lines #####
# Make a function to make series of buffers that from the
# lateral part of the grid
buffer_lines = function(shape, distance){
  buffer = st_buffer(x = shape, dist = distance) %>%
    mutate(lat_dist = distance - resolution/2)   
  return (buffer)
}

##### make_large_buffer #####
# Make a function to get large buffers used to tell left from right
make_large_buffer = function(distances = NULL,
                                 line = NULL){
  large_buffer = c(-max(distances), max(distances)) %>% 
    map(~buffer_side(line, .x)) %>% 
    do.call(rbind, .) %>% 
    st_intersection() %>% 
    filter(st_is(., c("POLYGON","MULTIPOLYGON"))) %>% 
    # Select minimal columns
    select(geometry, left_or_right)
  
  return(large_buffer)
}

##### buffer_side #####
# Make a function to make two bufffers that designate left or right side of centerline
buffer_side = function(shape, distance){
  buffer = st_buffer(x = shape, dist = distance, singleSide = T) %>%
    # Multiply by to 10 exagerate the number
    mutate(left_or_right = distance*10)   
  
  return(buffer)
}

##### make_sample_points #####
make_sample_points = function(resolution = resolution,
                              line = shape_files$line){
  sample_points = st_line_sample(x = line, density = 1/resolution) %>%
    # Conver from a list to a simple feature
    st_sf() %>%
    # Break out into points
    st_cast("POINT") %>% 
    # Assign each one a distance
    mutate(distance = 1:n()*resolution)
  return(sample_points)
}

##### make_vor_cells #####
make_vor_cells = function(points = NULL,
                          top = NULL,
                          resolution = NULL){
  vor_cells = st_voronoi(x = st_union(points)) %>%
    st_sf() %>%
    # Break up the geomerty collection to a polygon simple feature
    st_cast() %>% 
    # Join with the points to get the distance attribute form the sample_points layer
    st_join(points) %>%
    # Join to get the attribute form the top marker point
    st_join(top) %>% 
    # If we flip the distances make a row of the new distances
    # also make a row column which will just be 0 if no flip is necessary
    # but will have a value of we need to
    arrange(-distance) %>% 
    mutate(altDist = 1:n()*resolution,
           checkDist = distance - ifelse(is.na(place), distance, max(distance))) %>% 
    # Check to see if it needs to be flipped an if so do it
    flip_check("checkDist", "distance", "altDist") 
  
  return(vor_cells)
    
}

##### flip_check #####
# Make a function to check if the river distances are backwards
# and flip them if they are
flip_check = function(df, checkDist, distance, altDistance){
  if(abs(sum(df[[checkDist]]))!=0){
    df = df %>% 
      # use mutate_ because passing strings as col names
      mutate_(distance = altDist)
  }
  return(df)
}




# # Clip by the river domain
# st_intersection(buffers) %>% 
#   filter(st_is(., c("POLYGON","MULTIPOLYGON"))) %>% 
#   # cast everyting as a multi polygon then as a polygon to break multi parts into single parts
#   st_cast("MULTIPOLYGON") %>%
#   st_cast("POLYGON") %>% 
#   # Calculate the area of each
#   mutate(area = as.numeric(st_area(.))) %>% 
#   #filter out very small cells  
#   filter(!area<resolution^2/100) %>%
#   # Do a join with the large buffer to tell which side is left and right
#   st_join(large_buffer) %>% 
#   filter(!(lat_dist == 0 & left_or_right < 0)) %>% 
#   # filter out the curved end caps
#   filter(!(distance == max(distance) | distance == min(distance))) 

##### make_grid #####
make_grid = function(resolution = NULL,
                     cells = NULL,
                     buffers = NULL,
                     large_buffer = NULL){
  grid = cells %>% 
    st_intersection(buffers) %>% 
    filter(st_is(., c("POLYGON","MULTIPOLYGON"))) %>% 
    # cast everyting as a multi polygon then as a polygon to break multi parts into single parts
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>% 
    # Calculate the area of each
    mutate(area = as.numeric(st_area(.))) %>% 
    #filter out very small cells  
    filter(!area<resolution^2/100) %>%
    # Do a join with the large buffer to tell which side is left and right
    st_join(large_buffer) %>% 
    filter(!(lat_dist == 0 & left_or_right < 0)) %>% 
    # filter out the curved end caps
    filter(!(distance == max(distance) | distance == min(distance))) 
  
  return(grid)
}