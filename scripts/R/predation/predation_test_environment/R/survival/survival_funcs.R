
# select the survival bonus conveyed by distance to cover -----------------

# dis_to_cover - the value of distance to cover
# bonus_table - precalculated table of values that associates distance to cover with survival
get_cover_survival_bonus <- function(dis_to_cover, bonus_table){
  surv_bonus <- bonus_table %>% 
    filter(magnitude == as.character(dis_to_cover)) %>% # as.character is used because something dplyr interprets doubles as characters....
    pull(survival) %>%
    as.double()
  return(surv_bonus)
}

# calculated survival probability given a bonus and a base predation --------

# surv_bonus - bonus to survival conveyed by distance to cover
# pred_success - the base chance for a predator to succeed in a predation event; this is a reach value
get_survival_prob <- function(surv_bonus, pred_success){
  survival_prob <- 1 - pred_success + (pred_success * surv_bonus)
  return(survival_prob)
}