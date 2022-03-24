# parameters used to initialize the environment
build_environment_params <- list(
  interval = 5, # cell width in meters
  width = 100, # channel width in meters
  length = 10000, # transect length in meters
  veg_prob =  c(0.63, 0.10, 0.11, 0.09), # frequencies of veg absent, spare, medium, dense
  wood_prob = c(0.63, 0.26, 0.08, 0.03), # frequencies of woody material absent, spare, medium, dense
  veg_wood_density_bins = c(0, 0.05, 0.3, 0.5), # proportion sizes for "absent", "sparse", "medium" and "dense"
  velocity = c('slow', 'medium', 'fast'), # velocity categories
  shore_v_probs = c(0.6, .31, 0.09), # probability of the respective velocities occurring in the shore area
  channel_v_probs = c(0.33, 0.33, 0.33), # probability of the respective velocities occurring in the channel area
  depth = c('D1', 'D2', 'D3', 'D4'), # depth categories
  shore_d_probs = c(0.03, 0.29, 0.48, 0.21), # probability of the respective depths occurring in the shore area
  channel_d_probs = c(0.1, 0.11, 0.26, 0.62), # probability of the respective depths occurring in the channel area
  substrate = c('rocky', 'sand'), # substrate categories
  substrate_probs = c(0.5, 0.5) # probability of the respective substrate types occurring
)
