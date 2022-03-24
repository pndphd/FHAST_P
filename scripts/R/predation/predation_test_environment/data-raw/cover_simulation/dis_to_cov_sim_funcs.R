library(tidyverse) # for doing all the tidy stuff
library(sf) # for creating polygons
library(furrr) # for using the future versions of map
library(broom) # for nesting and such

##### function to generate the vertices of a regular polygon
# n_sides  - number of sides of the polygon
# area - area of the polygon
# x_offset, y_offset - coordinates to offset the polgyon center from 0,0
# max, min - the values bounding where the vertices can be
# id_num - used for indexing when there are multiple polygons generated

regular_poly <- function(n_sides, area, x_offset, y_offset, max, min, id_num)
{
  # Find the radius of the circumscribed circle
  radius <- calculate_radius(area, n_sides)
  
  # create an empty list for the vertices
  points <- list(x=NULL, y=NULL, id=NULL)
  
  # magnitude of each angle
  angles <- (2*pi)/n_sides * 1:n_sides
  
  # calculate the vertices and add to points list based on angle, radius, and offset
  points$x <- cos(angles) * radius + x_offset
  points$y <- sin(angles) * radius + y_offset
  
  # make sure points lie within the grid based on min, max
  # note that sometimes part of the polygon is cut off when vertices lie outside the bounds
  points$x <- ifelse(points$x > max, 
                     max, 
                     ifelse(points$x < min,
                            min, 
                            points$x))
  points$y <- ifelse(points$y > max, 
                     max, 
                     ifelse(points$y < min, 
                            min, 
                            points$y))
  
  # add the first point again to the end of the points list
  # this closes the polygon and is necessary for creating polygons in the sf package
  points$x[length(points$x) + 1] <- points$x[1]
  points$y[length(points$y) + 1] <- points$y[1]
  
  # add the id number
  points$id <- rep(id_num, length(points$x))
  
  return(points)
}

##### generate a specified number of polygons
# params is a list of parameters in the main script
# max_area is the largest area that any of the polygons can be
# n_poly is the desired number of polygons to generate
multi_col <- function(params, max_area, n_poly){
  # randomly pick a number of sides for the polygon based on values specified in params
  # function often breaks because of sample() when min and max number of sides are equal; if-else block handles that
  if(params$max_n_poly_sides - params$min_n_poly_sides > 0){
    n_sides <- sample(
      x = seq(
        from = params$min_n_poly_sides, 
        to = params$max_n_poly_sides,
        by = 1
      ), 
      size = n_poly, 
      replace = TRUE
    )
  } else{
    n_sides <- params$min_n_poly_sides
  }
  
  # assign areas for each polygon based on the maximum allowed value
  area <- sample(
    x = seq(
      from = max_area*10^-3, 
      to = max_area,
      by = max_area*10^-3
    ), 
    size = n_poly, 
    replace = TRUE
  )
  
  # create lists of values for x,y offsets
  x_offset <- sample(
    x = seq(
      from = 0,
      to = params$cell_size,
      by = params$cell_size*10^-3
    ),
    size = n_poly,
    replace = TRUE
  )
  
  y_offset <- sample(
    x = seq(
      from = 0,
      to = params$cell_size,
      by = params$cell_size*10^-3
    ),
    size = n_poly,
    replace = TRUE
  )
  
  # specify the min, max values of the coordinates of the vertices
  # based on the cell size
  max <- params$cell_size
  min <- 0
  
  # pick ID numbers for each polygon
  id_num <- 1:n_poly
  
  # create a dataframe of polygon vertices
  df <- pmap_dfr(
    .l = list(
      n_sides, 
      area, 
      x_offset, 
      y_offset, 
      max, 
      min, 
      id_num
      ), 
    .f = regular_poly
    )
  return(df)
}

##### use the sf package to turn polygon coordinates into actual polygons
# params is a list of parameters in the main script
# max_area is the largest area that any of the polygons can be
# n_poly is the desired number of polygons to generate
create_polygon <- function(params, max_area, n_poly){
  poly <- multi_col(params, max_area, n_poly) %>%
    st_as_sf(coords = c("x","y")) %>% # convert x,y coordinates into points readable by sf
    group_by(id) %>% # group each polygon
    summarise(geometry = st_combine(geometry)) %>% # turn the coordinates into geometry 
    st_cast("POLYGON") %>% # turn the geometry into an actual polygon
    ungroup() %>% 
    st_union() # combine all polygons into one large polygon (individual pieces can still be separated and treated as a single "polygon")
  return(poly)
}

#### make points readable by sf and measure each distance to the polygon
# poly - polygon from sf packages
# pts - dataframe with cols x and y to indicate coordinates
distance_from_polygon <- function(poly, pts){
  distances <- st_as_sf(pts, coords = c('x', 'y')) %>%
    st_distance(., poly) %>%
    tibble(distance = .)
  return(distances)
}

##### create random x,y coordinates that can be used as points from which to measure distances to the nearest part of a polygon
# params is a list of parameters in the main script
generate_fish_locs <- function(params){
  points <- tibble(
    x = sample(
      x = seq(
        from = 0, 
        to = params$cell_size, 
        by = params$cell_size*10^-3
      ), 
      size = params$num_fish, 
      replace = TRUE
    ),
    y = sample(
      x = seq(
        from = 0, 
        to = params$cell_size, 
        by = params$cell_size*10^-3
      ), 
      size = params$num_fish, 
      replace = TRUE
    )
  )
  return(points)
}

##### measure the mean distance to a polygon in a cell
# params is a list of parameters in the main script
# max_area is the largest area that any of the polygons can be
# n_poly is the desired number of polygons to generate
measure_mean_distance <- function(params, max_area, n_poly){
  poly <- create_polygon(params, max_area, n_poly)
  
  # create random x,y coordinates that can be used as points from which to measure distances to the nearest part of a polygon
  pts <- generate_fish_locs(params)
  
  # make points readable by sf and measure each distance to the polygon
  distances <- distance_from_polygon(poly, pts)
  
  # calculate the mean distance both including and excluding points that fell within the polygon itself
  distances <- distances %>%
    summarise(mean_dis = mean(distance),
              mean_dis_no_0 = mean(distance[distance > 0]))

  # create a list of relevant data points
  data <-  list(
    num_patches = n_poly, 
    max_area = max_area, 
    pct_cover = pct_cover(poly, params), 
    mean_dis_w_0 = distances %>% pull(mean_dis),
    mean_dis_no_0 = distances %>% pull(mean_dis_no_0)
  )
  return(data)
}

##### generates a dataset of percent cover and mean distance to cover
#  creates cells with unique combinations of polygons (in terms of num. and size of polygons) and finds the mean distance
# params is a list of parameters in the main script
get_cover_vs_dis <- function(params){
  # create a list of the number of polygons to use in each simulated cell
  # will allow for creating cells with 1 polygon up to the max number specified in params
  n_poly_list <- seq(
    from = 1, 
    to = params$max_n_poly, 
    by = 1
  )
  
  # create a list of possible maximum areas for polygons in a simulated cell
  # minimum value must be larger than 0; maximum goes up to 2x the cell size
  area_list <- seq(
    from = params$cell_size*10^-3, 
    to = params$max_area_multiplier*params$cell_size, 
    by = params$cell_size*10^-3
  )
  
  # creates a dataframe of every possible combination of specified number of polygons and max polygon area
  # thus, each simulated cell has a unique combination of number of polygons and maximum polygon areas
  poly_area_df <- expand.grid(
    n_poly = n_poly_list, 
    max_area = area_list
  )
  
  # simulate each cell and find the mean distance to cover polygons
  cover_vs_dis_df <- future_map2_dfr(
    .x = poly_area_df$max_area, 
    .y = poly_area_df$n_poly, 
    .f = measure_mean_distance, 
    params = params, 
    .options = furrr_options(seed=TRUE)
  )
  return(cover_vs_dis_df)
}


#### Helpers

##### simple function for calculating percent area (actually a proportion, but whatever)
# polygon - object generated by sf
# params is a list of parameters in the main script
pct_cover <- function(polygon, params){
  # calculate area of polygon
  area_poly <- st_area(polygon)
  # divide by area of the cell
  pct_cover <- area_poly / params$cell_size^2
  return(pct_cover)
}

##### calculate the radius of a circle with a given area based on the number of sides of the inscribed polygon
# area - area of the inscribed polygon
# n_sides - number of sides of the polygon
calculate_radius <- function(area, n_sides){
  radius <- sqrt((2*area)/(n_sides*sin((2*pi)/n_sides)))
  return(radius)
}
