library(tictoc)
library(tidyverse)
library(furrr)
library(predpackplus)
library(ggpubr)
plan(multisession)
lengths = seq(7, 15, by = 1)
reach_length_km = seq(1, 20, by=1)
tic()

df <- expand.grid(mean_fish_length = lengths, distance_traveled = reach_length_km) %>%
  arrange(mean_fish_length) %>%
  mutate(survival = future_pmap(list(number_of_fish = 500,mean_length=mean_fish_length,n_transects=distance_traveled),
                                survival_simulation_driver, 
                                
                                .options=furrr_options(seed=TRUE)))
toc()

df2 <- df %>%
  mutate(mean_fish_length = as.factor(mean_fish_length),
         distance_traveled = as.numeric(distance_traveled),
         #log_survival = log(as.numeric(survival) + min(as.numeric(survival)[as.numeric(survival) > 0]) / 100000))
         log_survival = log(as.numeric(survival))) %>%
  filter(survival > 0)



labels = c(
  "7" = "Mean length = 7 cm",
  "8" = "Mean length = 8 cm",
  "9" = "Mean length = 9 cm",
  "10" = "Mean length = 10 cm",
  "11" = "Mean length = 11 cm",
  "12" = "Mean length = 12 cm",
  "13" = "Mean length = 13 cm",
  "14" = "Mean length = 14 cm",
  "15" = "Mean length = 15 cm"
  
)





ggplot(df2, aes(x = distance_traveled, y = as.numeric(log_survival))) +
  geom_point() +
  #geom_smooth(method = "lm") +
  #stat_regline_equation(label.x = 1, label.y = -3.35, aes(label = ..eq.label..)) +
  #stat_regline_equation(label.x = 1, label.y = -3.85, aes(label = ..rr.label..)) +
  facet_wrap(~ mean_fish_length, labeller = as_labeller(labels)) +
  
  labs(title = "Log survival vs. distance traveled (km) per size class (cm)",
       x = "Distance traveled (km)",
       y = "Survival (log)") +
  theme(panel.spacing=unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))


pred <- get_pred_positions(n_transects = 5)
graph_pred_positions(pred)
raster <- create_stream_raster_frame(pred, n_transects = 5)
enc <- calc_enc_probs(df = raster)
graph_enc_probs(enc)

