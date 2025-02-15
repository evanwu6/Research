# Libraries ####
library(tidyverse)
library(readxl)
library(ggforce)
library(concaveman)
library(knitr)
library(olsrr)
library(ranger)
library(Metrics)

set.seed(3630)

# Data ####
seasonal <- read_csv("CSVs/season_stats.csv")

pitchers <- read_csv("CSVs/pitcher_comps.csv")

arsenal <- read_csv("CSVs/arsenal.csv")


# Functions ####

# Strike Zone GG Object
geom_zone <- function(top = 11/3, bottom = 3/2, linecolor = "black"){
  geom_rect(xmin = -.7083, xmax = .7083, ymin = bottom, ymax = top,
            alpha = 0, color = linecolor, linewidth = 0.75)
}

# c(0, 0, -.25, -.5, -.25))

# Home Plate GG Object
geom_plate <- function(pov = "pitcher"){
  df <- case_when(
    pov == "pitcher" ~ 
      data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, .25, .5, .25)),
    pov == "catcher" ~ 
      data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, -.25, -.5, -.25))
  )
  
  g <- geom_polygon(data = df, aes(x = x, y = y), fill = "white", color = "black", linewidth = 1.25)
  g
}

# Barrel Function
is.barrel <- function(LA, EV){
  upper <- 1.11*EV - 78.89
  lower <- -EV + 124
  outcome <- (LA >= lower) & (LA <= upper) & (EV >= 98) & (LA >= 8) & (LA <= 50)
  outcome <- replace_na(outcome, FALSE)
  outcome
}

# Normal Name Changer
swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}

# Linear Regression With/Without Interaction Terms ####

# Model Data (Pitch = Slider, Pitching Hand = Right)
model_data <- arsenal %>% 
  filter(pitch_type == "SL",
         pitch_hand == "R") %>% 
  mutate(ovr_break = sqrt(pitcher_break_x^2 + pitcher_break_z^2))

# Simple Linear Regression
lm_simple <- lm(xwOBA ~ 
                  pitch_speed + spin_rate + pitcher_break_x + pitcher_break_z +
                  pitch_usage + ovr_break,
                data = model_data)

summary(lm_simple)


# Overview of all model combinations
model_all <- ols_step_all_possible(lm_simple)

# Backwards Elimination
lm_simple %>% ols_step_backward_p(penter = 0.2)

# Stepwise Selection
lm_simple %>% ols_step_both_p(prem = 0.15, pent = 0.15)

# New Model
lm1 <- lm(xwOBA ~ 
            ovr_break + pitch_usage + pitch_speed,
          data = model_data)

lm1 %>% 
  summary()

# Interaction Linear Regression
lm_interact <- lm(xwOBA ~ 
                    pitch_speed + spin_rate + pitcher_break_x + pitcher_break_z + pitch_usage + 
                    ovr_break + 
                    pitch_speed*spin_rate + pitch_speed*pitch_usage + pitch_speed*ovr_break +
                    pitch_speed*pitcher_break_x + pitch_speed*pitcher_break_z + 
                    spin_rate*ovr_break + spin_rate*pitcher_break_x + spin_rate*pitcher_break_z +
                    pitch_usage*ovr_break + pitch_usage*spin_rate,
                  data = model_data)

summary(lm_interact)


# model_interact_all <- ols_step_all_possible(lm_interact)

# Stepwise Selection
lm_interact %>% ols_step_both_p(pent = 0.15, prem = 0.05)

# Output removes ALL interactions for p < 0.05
# Keeps same as simple LM pitcher_break_z + pitch_speed + pitch_usage

lm_interact %>% ols_step_both_p(pent = 0.15, prem = 0.10)

# New Model with Interactions (p -value < 0.10 threshhold)
lm2 <- lm(xwOBA ~ 
            pitch_speed + pitch_usage + 
            pitch_speed*ovr_break + pitch_speed*pitcher_break_z,
          data = model_data)

lm2 %>% 
  summary()

# Trimmed Data
model_results <- model_data %>% 
  select(first_name, last_name, 
         pitch_speed, pitch_usage, pitcher_break_z, ovr_break, xwOBA)

# Comparing Model Predictions
# lm1 = simple
# lm2 = interactions

model_results <- model_results %>% 
  mutate(lm1 = predict(lm1, model_results)) %>% 
  mutate(lm2 = predict(lm2, model_results))

# R and RMSE of Simple Linear Model
with(model_results, cor(xwOBA, lm1))
with(model_results, rmse(xwOBA, lm1))

# R and RMSE of Interactions Linear Model
with(model_results, cor(xwOBA, lm2))
with(model_results, rmse(xwOBA, lm2))

model_results %>% 
  select(xwOBA, lm1, lm2) %>%
  pivot_longer(cols = lm1:lm2, 
               names_to = "model",
               values_to = "pred") %>% 
  mutate(model = str_replace(model, "lm1", "Simple LM"),
         model = str_replace(model, "lm2", "Interaction LM")) %>%
  ggplot(aes(x = xwOBA, y = pred, color = model)) +
  geom_point(shape = 18, size = 1.5, alpha = 0.75) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("navyblue", "skyblue")) +
  theme_classic() +
  labs(title = "Linear Models for RHP",
       x = "Observed",
       y = "Predicted",
       color = "Model")

# Model Data (Pitch = Slider, Pitching Hand = Right)
model_data_l <- arsenal %>% 
  filter(pitch_type == "SL",
         pitch_hand == "L") %>% 
  mutate(ovr_break = sqrt(pitcher_break_x^2 + pitcher_break_z^2))

# Simple Linear Regression
lm_simple_l <- lm(xwOBA ~ 
                    pitch_speed + spin_rate + pitcher_break_x + pitcher_break_z +
                    pitch_usage + ovr_break,
                  data = model_data_l)

summary(lm_simple_l)


# Overview of all model combinations
model_all_l <- ols_step_all_possible(lm_simple_l)

# Backwards Elimination
lm_simple_l %>% ols_step_backward_p(penter = 0.15)

# Stepwise Selection
lm_simple_l %>% ols_step_both_p(prem = 0.15, pent = 0.15)

# New Model
lm1_l <- lm(xwOBA ~ 
              pitcher_break_x + pitcher_break_z +
              ovr_break,
            data = model_data_l)

lm1_l %>% 
  summary()

# Model Data (Pitch = Slider, Pitching Hand = Left)

# Interaction Linear Regression
lm_interact_l <- lm(xwOBA ~ 
                      pitch_speed + spin_rate + pitcher_break_x + pitcher_break_z + pitch_usage + 
                      ovr_break + 
                      pitch_speed*spin_rate + pitch_speed*pitch_usage + pitch_speed*ovr_break +
                      pitch_speed*pitcher_break_x + pitch_speed*pitcher_break_z + 
                      spin_rate*ovr_break + spin_rate*pitcher_break_x + spin_rate*pitcher_break_z +
                      pitch_usage*ovr_break + pitch_usage*spin_rate,
                    data = model_data_l)

summary(lm_interact_l)


# model_interact_all <- ols_step_all_possible(lm_interact)

# Stepwise Selection
lm_interact_l %>% ols_step_both_p(pent = 0.15, prem = 0.05)

# Output removes ALL interactions for p < 0.05
# Keeps same as simple LM pitcher_break_z + pitch_speed + pitch_usage

lm_interact_l %>% ols_step_both_p(pent = 0.20, prem = 0.05)

# New Model with Interactions (p -value < 0.10 threshhold)
lm2_l <- lm(xwOBA ~ 
              spin_rate*pitcher_break_z,
            data = model_data_l)

lm2_l %>% 
  summary()

# Trimmed Data
model_results_l <- model_data_l %>% 
  select(first_name, last_name, 
         pitch_speed, pitch_usage, pitcher_break_z, ovr_break, xwOBA)

# Comparing Model Predictions
# lm1 = simple
# lm2 = interactions

model_results_l <- model_results_l %>% 
  mutate(lm1 = predict(lm1, model_results_l)) %>% 
  mutate(lm2 = predict(lm2, model_results_l))

# R and RMSE of Simple Linear Model
with(model_results_l, cor(xwOBA, lm1))
with(model_results_l, rmse(xwOBA, lm1))

# R and RMSE of Interactions Linear Model
with(model_results_l, cor(xwOBA, lm2))
with(model_results_l, rmse(xwOBA, lm2))

model_results_l %>% 
  select(xwOBA, lm1, lm2) %>%
  pivot_longer(cols = lm1:lm2, 
               names_to = "model",
               values_to = "pred") %>% 
  mutate(model = str_replace(model, "lm1", "Simple LM"),
         model = str_replace(model, "lm2", "Interaction LM")) %>%
  ggplot(aes(x = xwOBA, y = pred, color = model)) +
  geom_point(shape = 18, size = 1.5, alpha = 0.75) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("navyblue", "skyblue")) +
  theme_classic() +
  labs(title = "Linear Models for LHP",
       x = "Observed",
       y = "Predicted",
       color = "Model")

# Pitch by Pitch Data (Sliders)
pitches <- pitchers %>% 
  filter(pitch_type == "SL") %>% 
  mutate(pfx_x = pfx_x*12,
         pfx_z = pfx_z*12,
         ovr_break = round(sqrt(pfx_x^2 + pfx_z^2), 3))

# Simple Linear Regression
lm_pitches <- lm(run_exp_added ~ 
                   pitch_speed + release_spin_rate + pfx_x + pfx_z +
                   ovr_break + release_extension,
                 data = pitches)

summary(lm_pitches)


# Stepwise Selection
lm_pitches %>% ols_step_both_p(prem = 0.25, pent = 0.15)

# New Model
lm1_pitches <- lm(run_exp_added ~ 
                    release_spin_rate + pfx_x + release_extension,
                  data = pitches)

lm1_pitches %>% 
  summary()
```

```{r Sample Pitch Linear Model Interactions, echo = FALSE, warning = FALSE}
# Model Data (Pitch = Slider)

# Interaction Linear Regression
lm_interact_pitches <- lm(run_exp_added ~ 
                            pitch_speed + release_spin_rate + pfx_x + pfx_z +
                            ovr_break + release_extension +
                            pitch_speed*release_spin_rate + pitch_speed*pfx_x + pitch_speed*pfx_z +
                            pitch_speed*ovr_break + pitch_speed*release_extension + 
                            release_spin_rate*pfx_x + release_spin_rate*pfx_z + 
                            release_spin_rate*ovr_break + release_spin_rate*release_extension +
                            release_extension*pfx_x + release_extension*pfx_z + release_extension*ovr_break,
                          data = pitches)

summary(lm_interact_pitches)


# model_interact_all <- ols_step_all_possible(lm_interact)

# Stepwise Selection
lm_interact_pitches %>% ols_step_both_p(pent = 0.15, prem = 0.15)

lm2_pitches <- lm(run_exp_added ~ 
                    release_spin_rate*pfx_x + release_extension,
                  data = pitches)

lm2_pitches %>% 
  summary()
```

```{r Sample Pitch LM Comparisons, echo = FALSE}
# Comparing Model Predictions
# lm1 = simple
# lm2 = interactions available but not used

model_results_pitches <- pitches %>% 
  select(run_exp_added, pitch_speed, release_spin_rate, pfx_x, pfx_z,
         release_extension) %>% 
  mutate(lm1 = predict(lm1_pitches, pitches),
         lm2 = predict(lm2_pitches, pitches))

# R and RMSE of Simple Linear Model

model_results_pitches %>% 
  filter(!is.na(release_spin_rate),
         !is.na(pfx_x),
         !is.na(release_extension),
         !is.na(run_exp_added)) %>% 
  with(cor(run_exp_added, lm1))

model_results_pitches %>% 
  filter(!is.na(release_spin_rate),
         !is.na(pfx_x),
         !is.na(release_extension),
         !is.na(run_exp_added)) %>% 
  with(rmse(run_exp_added, lm1))

# R and RMSE of Interaction Linear Model
model_results_pitches %>% 
  filter(!is.na(release_spin_rate),
         !is.na(pfx_x),
         !is.na(release_extension),
         !is.na(run_exp_added)) %>% 
  with(cor(run_exp_added, lm2))

model_results_pitches %>% 
  filter(!is.na(release_spin_rate),
         !is.na(pfx_x),
         !is.na(release_extension),
         !is.na(run_exp_added)) %>% 
  with(rmse(run_exp_added, lm2))

# Graph
model_results_pitches %>% 
  select(run_exp_added, lm1, lm2) %>%
  pivot_longer(cols = lm1:lm2, 
               names_to = "model",
               values_to = "pred") %>% 
  mutate(model = str_replace(model, "lm1", "Simple LM"),
         model = str_replace(model, "lm2", "Interaction LM")) %>%
  ggplot(aes(x = run_exp_added, y = pred, color = model)) +
  geom_point(shape = 18, size = 1.5, alpha = 0.75) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("navyblue", "skyblue")) +
  theme_classic() +
  labs(title = "Linear Models for Pitch-by-Pitch Data",
       subtitle = "Predicting Run Expectancy Added",
       caption = "Pitchers: Scherzer, Taillon, Keller, Manoah, Gallen, Garcia, Gray",
       x = "Observed",
       y = "Predicted",
       color = "Model")
