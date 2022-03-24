pred_params  <- list(
  cover_types = c('wood', 'veg', 'open'), # types of cover
  preds_km = 300, # number of predators per km in the system
  pred_react_distance = 1, # distance at which predators react to prey
  cover_weights = list( # effect of cover type on the probability of predator presence
    wood = 24, 
    veg = -5, 
    open = 0),
  condition_types = c('velocity', 'substrate', 'depth'), # categories of environmental conditions
  condition_weights = list( # effects of environmental conditions on the probability of predator presence
    velocity = list(
      slow = 0, 
      medium = 2, 
      fast = -5),
    substrate = list(
      rocky = 9, 
      sand = -9),
    depth = list(
      D1 = 0, 
      D2 = -3, 
      D3 = -7, 
      D4 = -10)
  )
)
