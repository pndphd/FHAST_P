#### important parameters used in the worker and helper functions of the simulation
cover_sim_params <- list(
  cell_size = 1, # size of each cell; value isn't really important
  min_n_poly_sides = 3, # minimum number of sides a polygon can have; going below 3 is not recommended....
  max_n_poly_sides = 10, # max number of sides a polygon can have; feel free to play around with it; min and max can be the same if you like
  max_n_poly = 4, # maximum number of polygons that will be generated in any one cell
  num_fish = 10000, # "fish" are just the random points generated in a cell and used to measure distances to the polygons
  max_area_multiplier = 3
)