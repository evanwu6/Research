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

all_pitches <- read_csv("CSVs/all_pitches.csv")


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



####


rhp <- all_pitches %>% 
  filter(p_throws == "R")

# Batter Whiff
batter_stats <- rhp %>% 
  summarize(rate = mean(whiff),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_whiff = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bwhiff = (pitches / 300)*rate + ((300-pitches)/300)*pitch_whiff) %>% 
  mutate(pred_bwhiff = ifelse(pitches >= 300, rate, pred_bwhiff))


# Merging Whiff Prediction with RHP
rhp <- rhp %>% 
  left_join(select(batter_stats, batter, pitch_type, pred_bwhiff), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))

rhp <- rhp %>% 
  mutate(prev_pitch = ifelse(is.na(prev_pitch), "None", prev_pitch)) %>% 
  mutate(Count = paste0(balls, "-", strikes)) %>% 
  mutate(Count = as.factor(Count))

# Adding Proportional Distance
rhp <- rhp %>% 
  mutate(dist_x = plate_x/0.708333,
         center = (sz_top + sz_bot) / 2,
         dist_z = ifelse(plate_z >= center, 
                         (plate_z - center) / (sz_top - center), 
                         (plate_z - center) / (center - sz_bot)),
         dist_prop = sqrt(dist_z^2 + dist_x^2))


# Barrel
batter_stats2 <- rhp %>% 
  summarize(rate = mean(is_barrel),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_barrel = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bbarrel = (pitches / 300)*rate + ((300-pitches)/300)*pitch_barrel) %>% 
  mutate(pred_bbarrel = ifelse(pitches >= 300, rate, pred_bbarrel))

rhp <- rhp %>% 
  left_join(select(batter_stats2, batter, pitch_type, pred_bbarrel), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))


# Strike
batter_stats3 <- rhp %>% 
  summarize(rate = mean(is_strike),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_strike = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bstrike = (pitches / 300)*rate + ((300-pitches)/300)*pitch_strike) %>% 
  mutate(pred_bstrike = ifelse(pitches >= 300, rate, pred_bstrike))

rhp <- rhp %>% 
  left_join(select(batter_stats3, batter, pitch_type, pred_bstrike), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))

sliders <- rhp %>% 
  filter(hitter == "R") %>% 
  filter(pitch_type == "SL") %>% 
  mutate(count = paste0(balls, "_", strikes),
         prev_pitch = as.factor(prev_pitch)) %>% 
  mutate(prev_pitch_ff = ifelse(prev_pitch == "FF", 1, 0))

fastballs <- rhp %>% 
  filter(hitter == "R") %>% 
  filter(pitch_type == fb_type) %>% 
  mutate(count = paste0(balls, "_", strikes),
         prev_pitch = as.factor(prev_pitch))


# LHP
lhp <- all_pitches %>% 
  filter(p_throws == "L")


# Batter Whiff
batter_stats_l <- lhp %>% 
  summarize(rate = mean(whiff),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_whiff = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bwhiff = (pitches / 300)*rate + ((300-pitches)/300)*pitch_whiff) %>% 
  mutate(pred_bwhiff = ifelse(pitches >= 300, rate, pred_bwhiff))


# Merging Whiff Prediction with LHP
lhp <- lhp %>% 
  left_join(select(batter_stats_l, batter, pitch_type, pred_bwhiff), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))

lhp <- lhp %>% 
  mutate(prev_pitch = ifelse(is.na(prev_pitch), "None", prev_pitch)) %>% 
  mutate(Count = paste0(balls, "-", strikes)) %>% 
  mutate(Count = as.factor(Count))

# Adding Proportional Distance
lhp <- lhp %>% 
  mutate(dist_x = plate_x/0.708333,
         center = (sz_top + sz_bot) / 2,
         dist_z = ifelse(plate_z >= center, 
                         (plate_z - center) / (sz_top - center), 
                         (plate_z - center) / (center - sz_bot)),
         dist_prop = sqrt(dist_z^2 + dist_x^2))


# Barrel
batter_stats2_l <- lhp %>% 
  summarize(rate = mean(is_barrel),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_barrel = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bbarrel = (pitches / 300)*rate + ((300-pitches)/300)*pitch_barrel) %>% 
  mutate(pred_bbarrel = ifelse(pitches >= 300, rate, pred_bbarrel))

lhp <- lhp %>% 
  left_join(select(batter_stats2_l, batter, pitch_type, pred_bbarrel), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))


# Strike
batter_stats3_l <- lhp %>% 
  summarize(rate = mean(is_strike),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_strike = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bstrike = (pitches / 300)*rate + ((300-pitches)/300)*pitch_strike) %>% 
  mutate(pred_bstrike = ifelse(pitches >= 300, rate, pred_bstrike))

lhp <- lhp %>% 
  left_join(select(batter_stats3_l, batter, pitch_type, pred_bstrike), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))


### Four Seam Fastballs RHP to RHH

##### Whiff - Removed: pfx_x
##### Barrel - Removed: pfx_z, dist_prop, pfx_x, pfx_total, zone (multiple at 0.05-0.06)
##### Strike - Removed: pitch_speed, pfx_x, pfx_total

fb_r <- rhp %>% 
  filter(pitch_type == "FF",
         hitter == "R")


# Whiff ####

# Refined Model
fb_r_whiff <- glm(whiff ~ pitch_speed + pfx_z + pfx_total + zone + dist_x + dist_z +
                    dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bwhiff,
                  data = fb_r, family = binomial)

# Removed: pfx_x

# Model Evaluation
summary(fb_r_whiff)

fb_r_whiff_test <- augment(fb_r_whiff, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "whiff",
         Hand = "right",
         pitch = "fastball")


rmse(fb_r_whiff_test$prediction, fb_r_whiff_test$whiff)



# Barrel ####

# Refined Model
fb_r_barrel <- glm(is_barrel ~ pitch_speed + dist_x + dist_z + zone +
                     I(dist_x^2) + I(dist_z^2) + pred_bbarrel,
                   data = fb_r, family = binomial)

# Removed: pfx_x, dist_prop, pfx_total, pfx_z, release_spin_rate

# Model Evaluation
summary(fb_r_barrel)

fb_r_barrel_test <- augment(fb_r_barrel, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "barrel",
         Hand = "right",
         pitch = "fastball")


rmse(fb_r_barrel_test$prediction, fb_r_barrel_test$is_barrel)




# Strike ####

# Refined Model
fb_r_strike <- glm(is_strike ~ pfx_z + zone + dist_x + dist_z + dist_prop + 
                     I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bstrike,
                   data = fb_r, family = binomial)


fb_r_strike_formula <- step(fb_r_strike, direction = "backward")
fb_r_strike_formula$formula

# Removed: pitch_speed, pfx_x, pfx_total

# Model Evaluation

fb_r_strike_test <- augment(fb_r_strike, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>%
  mutate(Response = "strike",
         Hand = "right",
         pitch = "fastball")


rmse(fb_r_strike_test$prediction, fb_r_strike_test$is_strike)




### Four Seam Fastballs RHP to LHH

##### Whiff - Removed: pfx_x, pfx_z
##### Barrel - Removed: release_spin_rate + pfx_total
##### Strike - Removed: pfx_x, pfx_z

fb_l <- rhp %>% 
  filter(pitch_type == "FF",
         hitter == "L")


# Whiff ####

# Refined Model
fb_l_whiff <- glm(whiff ~ pitch_speed + pfx_total + zone + dist_x + dist_z +
                    dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bwhiff,
                  data = fb_l, family = binomial)

# Removed: pfx_x, pfx_z

# Model Evaluation
# summary(fb_l_whiff)

fb_l_whiff_test <- augment(fb_l_whiff, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "whiff",
         Hand = "left",
         pitch = "fastball")


rmse(fb_l_whiff_test$prediction, fb_l_whiff_test$whiff)




# Barrel ####

# Refined Model
fb_l_barrel <- glm(is_barrel ~ pitch_speed + pfx_x + zone + dist_x + 
                     dist_z + I(dist_x^2) + I(dist_z^2) +  pred_bbarrel,
                   data = fb_l, family = binomial)

# Removed: release_spin_rate + pfx_total

# Model Evaluation
# summary(fb_l_barrel)

fb_l_barrel_test <- augment(fb_l_barrel, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "barrel",
         Hand = "left",
         pitch = "fastball")


rmse(fb_l_barrel_test$prediction, fb_l_barrel_test$is_barrel)





# Strike ####

# Refined Model
fb_l_strike <- glm(is_strike ~ pitch_speed + pfx_total + zone + dist_x + dist_z +
                     dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bstrike,
                   data = fb_l, family = binomial)

# Removed: pfx_x, pfx_z

# Model Evaluation
# summary(fb_l_strike)

fb_l_strike_test <- augment(fb_l_strike, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "strike",
         Hand = "left",
         pitch = "fastball")


rmse(fb_l_strike_test$prediction, fb_l_strike_test$is_strike)



### Sliders RHP to RHH

##### Whiff - Removed: pitch_speed
##### Barrel - Removed: release_spin_rate, pfx_total, dist_prop, zone, pfx_x, pfx_z, pitch_speed
##### Strike - Removed: None

sl_r <- rhp %>% 
  filter(pitch_type == "SL",
         hitter == "R")


# Whiff ####

# Refined Model
sl_r_whiff <- glm(whiff ~ pfx_x + pfx_z + pfx_total + zone + dist_x + dist_z +
                    dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bwhiff,
                  data = sl_r, family = binomial)

# Removed: pitch_speed

# Model Evaluation
# summary(sl_r_whiff)

sl_r_whiff_test <- augment(sl_r_whiff, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "whiff",
         Hand = "right",
         pitch = "slider")


rmse(sl_r_whiff_test$prediction, sl_r_whiff_test$whiff)





# Barrel ####

# Refined Model
sl_r_barrel <- glm(is_barrel ~ dist_x + dist_z +
                     I(dist_x^2) + I(dist_z^2) + pred_bbarrel,
                   data = sl_r, family = binomial)

# Removed: release_spin_rate, pfx_total, dist_prop, zone, pfx_x, pfx_z, pitch_speed

# Model Evaluation
# summary(sl_r_barrel)

sl_r_barrel_test <- augment(sl_r_barrel, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "barrel",
         Hand = "right",
         pitch = "slider")


rmse(sl_r_barrel_test$prediction, sl_r_barrel_test$is_barrel)





# Strike ####

# Refined Model
sl_r_strike <- glm(is_strike ~ pitch_speed + pfx_x + pfx_z + pfx_total + zone + dist_x + dist_z +
                     dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bstrike,
                   data = sl_r, family = binomial)

# Removed: None

# Model Evaluation
# summary(sl_r_strike)

sl_r_strike_test <- augment(sl_r_strike, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "strike",
         Hand = "right",
         pitch = "slider")


rmse(sl_r_strike_test$prediction, sl_r_strike_test$is_strike)






### Sliders RHP to LHH

##### Whiff - Removed: pfx_z, pfx_x, pfx_total
##### Barrel - Removed: pfx_total, dist_prop, release_spin_rate
##### Strike - Removed: pfx_z, release_spin_rate, pitch_speed

sl_l <- rhp %>% 
  filter(pitch_type == "SL",
         hitter == "L")


# Whiff ####

# Refined Model
sl_l_whiff <- glm(whiff ~ pitch_speed + zone + dist_x + dist_z +
                    dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bwhiff,
                  data = sl_l, family = binomial)

# Removed: pfx_z, pfx_x, pfx_total

# Model Evaluation
# summary(sl_l_whiff)

sl_l_whiff_test <- augment(sl_l_whiff, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "whiff",
         Hand = "left",
         pitch = "slider")

rmse(sl_l_whiff_test$prediction, sl_l_whiff_test$whiff)





# Barrel ####

# Refined Model
sl_l_barrel <- glm(is_barrel ~ pitch_speed + pfx_x + pfx_z + zone + dist_x + dist_z +
                     I(dist_x^2) + I(dist_z^2) + pred_bbarrel,
                   data = sl_l, family = binomial)

# Removed: pfx_total, dist_prop, release_spin_rate

# Model Evaluation
# summary(sl_l_barrel)

sl_l_barrel_test <- augment(sl_l_barrel, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "barrel",
         Hand = "left",
         pitch = "slider")


rmse(sl_l_barrel_test$prediction, sl_l_barrel_test$is_barrel)







# Strike ####

# Refined Model
sl_l_strike <- glm(is_strike ~ pfx_x + pfx_total + zone + dist_x + dist_z +
                     dist_prop + I(dist_x^2) + I(dist_z^2) + pred_bstrike,
                   data = sl_l, family = binomial)

# Removed: pfx_z, release_spin_rate, pitch_speed

# Model Evaluation
# summary(sl_l_strike)

sl_l_strike_test <- augment(sl_l_strike, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "strike",
         Hand = "left",
         pitch = "slider")


rmse(sl_l_strike_test$prediction, sl_l_strike_test$is_strike)


### Curveball RHP to RHH

##### Whiff - Removed: pitch_speed, pfx_x
##### Barrel - Removed: pitch_speed, pfx_x
##### Strike - Removed: pitch_speed, pfx_x, pfx_total

cu_r <- rhp %>% 
  filter(pitch_type == "CU",
         hitter == "R")


# Whiff ####

# Refined Model
cu_r_whiff <- glm(whiff ~ pfx_z + pfx_total + zone + dist_x + dist_z + dist_prop + 
                    I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bwhiff,
                  data = cu_r, family = binomial)

cu_r_whiff_formula <- step(cu_r_whiff, direction = "backward")
cu_r_whiff_formula$formula

# Removed: pitch_speed, pfx_x


cu_r_whiff_test <- augment(cu_r_whiff, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "whiff",
         Hand = "right",
         pitch = "curveball")


rmse(cu_r_whiff_test$prediction, cu_r_whiff_test$whiff)



# Barrel ####

# Refined Model
cu_r_barrel <- glm(is_barrel ~ pfx_z + pfx_total + zone + dist_x + dist_z +
                     dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bbarrel,
                   data = cu_r, family = binomial)


# Removed: pitch_speed, pfx_x



cu_r_barrel_test <- augment(cu_r_barrel, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "barrel",
         Hand = "right",
         pitch = "curveball")


rmse(cu_r_barrel_test$prediction, cu_r_barrel_test$is_barrel)




# Strike ####

# Refined Model
cu_r_strike <- glm(is_strike ~ pfx_z + zone + dist_x + dist_z +
                     dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bstrike,
                   data = cu_r, family = binomial)


cu_r_strike_formula <- step(cu_r_strike, direction = "backward")
cu_r_strike_formula$formula

# Removed: pitch_speed, pfx_x, pfx_total


cu_r_strike_test <- augment(cu_r_strike, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>%
  mutate(Response = "strike",
         Hand = "right",
         pitch = "curveball")


rmse(cu_r_strike_test$prediction, cu_r_strike_test$is_strike)


### Curveball RHP to LHH

##### Whiff - Removed: pfx_x, pfx_z, pfx_total
##### Barrel - Removed: pfx_x, zone, pfx_z, release_spin_rate, dist_prop, pitch_speed
##### Strike - Removed: pitch_speed, pfx_x, pfx_z

cu_l <- rhp %>% 
  filter(pitch_type == "CU",
         hitter == "L")


# Whiff ####

# Refined Model
cu_l_whiff <- glm(whiff ~ pitch_speed + zone + dist_x + dist_z + dist_prop + 
                    I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bwhiff,
                  data = cu_l, family = binomial)


cu_l_whiff_formula <- step(cu_l_whiff, direction = "backward")
cu_l_whiff_formula$formula

# Removed: pfx_x, pfx_z, pfx_total


cu_l_whiff_test <- augment(cu_l_whiff, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "whiff",
         Hand = "left",
         pitch = "curveball")


rmse(cu_l_whiff_test$prediction, cu_l_whiff_test$whiff)



# Barrel ####

# Refined Model
cu_l_barrel <- glm(is_barrel ~ pfx_total + dist_x + dist_z +
                     I(dist_x^2) + I(dist_z^2) + pred_bbarrel,
                   data = cu_l, family = binomial)

cu_l_barrel_formula <- step(cu_l_barrel, direction = "backward")
cu_l_barrel_formula$formula

# Removed: pfx_x, zone, pfx_z, release_spin_rate, dist_prop, pitch_speed



cu_l_barrel_test <- augment(cu_l_barrel, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "barrel",
         Hand = "left",
         pitch = "curveball")


rmse(cu_l_barrel_test$prediction, cu_l_barrel_test$is_barrel)




# Strike ####

# Refined Model
cu_l_strike <- glm(is_strike ~ pfx_total + zone + dist_x + dist_z + dist_prop + 
                     I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bstrike,
                   data = cu_l, family = binomial)


cu_l_strike_formula <- step(cu_l_strike, direction = "backward")
cu_l_strike_formula$formula

# Removed: pitch_speed, pfx_x, pfx_z


cu_l_strike_test <- augment(cu_l_strike, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>%
  mutate(Response = "strike",
         Hand = "left",
         pitch = "curveball")


rmse(cu_l_strike_test$prediction, cu_l_strike_test$is_strike)



### Change Ups RHP to RHH

##### Whiff - Removed: release_spin_rate, pfx_x, pfx_z
##### Barrel - Removed: release_spin_rate, pitch_speed, zone, dist_prop, pfx_x
##### Strike - Removed: release_spin_rate, pfx_x

ch_r <- rhp %>% 
  filter(pitch_type == "CH",
         hitter == "R")


# Whiff ####

# Refined Model
ch_r_whiff <- glm(whiff ~ pitch_speed + pfx_total + zone + dist_x + dist_z +
                    dist_prop + I(dist_x^2) + I(dist_z^2) + pred_bwhiff,
                  data = ch_r, family = binomial)


# Removed: release_spin_rate, pfx_x, pfx_z

# Model Evaluation
summary(ch_r_whiff)

ch_r_whiff_test <- augment(ch_r_whiff, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "whiff",
         Hand = "right",
         pitch = "changeup")


rmse(ch_r_whiff_test$prediction, ch_r_whiff_test$whiff)



# Barrel ####

# Refined Model
ch_r_barrel <- glm(is_barrel ~ pfx_z + pfx_total + dist_x + dist_z +
                     I(dist_x^2) + I(dist_z^2) + pred_bbarrel,
                   data = ch_r, family = binomial)


# Removed: release_spin_rate, pitch_speed, zone, dist_prop, pfx_x

# Model Evaluation
summary(ch_r_barrel)

ch_r_barrel_test <- augment(ch_r_barrel, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "barrel",
         Hand = "right",
         pitch = "changeup")


rmse(ch_r_barrel_test$prediction, ch_r_barrel_test$is_barrel)




# Strike ####

# Refined Model
ch_r_strike <- glm(is_strike ~ pitch_speed + pfx_z + pfx_total + zone + dist_x + dist_z +
                     dist_prop + I(dist_x^2) + I(dist_z^2) + pred_bstrike,
                   data = ch_r, family = binomial)


# Removed: release_spin_rate, pfx_x

# Model Evaluation
# summary(ch_r_strike)

ch_r_strike_test <- augment(ch_r_strike, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>%
  mutate(Response = "strike",
         Hand = "right",
         pitch = "changeup")


rmse(ch_r_strike_test$prediction, ch_r_strike_test$is_strike)



### Change Ups RHP to LHH

##### Whiff - Removed: 
##### Barrel - Removed: 
##### Strike - Removed: 

ch_l <- rhp %>% 
  filter(pitch_type == "CH",
         hitter == "L")


# Whiff ####

# Refined Model
ch_l_whiff <- glm(whiff ~ pitch_speed + pfx_z + zone + dist_x + dist_z + dist_prop + 
                    I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bwhiff,
                  data = ch_l, family = binomial)


ch_l_whiff_formula <- step(ch_l_whiff, direction = "backward")
ch_l_whiff_formula$formula

# Removed: pfx_total, pfx_x

# Model Evaluation
summary(ch_l_whiff)

ch_l_whiff_test <- augment(ch_l_whiff, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "whiff",
         Hand = "left",
         pitch = "changeup")


rmse(ch_l_whiff_test$prediction, ch_l_whiff_test$whiff)



# Barrel ####

# Refined Model
ch_l_barrel <- glm(is_barrel ~ dist_x + dist_z + I(dist_x^2) + I(dist_z^2) + pred_bbarrel,
                   data = ch_l, family = binomial)


# Removed: pfx_x, release_spin_rate, pitch_speed, pfx_total, dist_prop, pfx_z, zone

# Model Evaluation
summary(ch_l_barrel)

ch_l_barrel_test <- augment(ch_l_barrel, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>% 
  mutate(Response = "barrel",
         Hand = "left",
         pitch = "changeup")


rmse(ch_l_barrel_test$prediction, ch_l_barrel_test$is_barrel)




# Strike ####

# Refined Model
ch_l_strike <- glm(is_strike ~ pitch_speed + pfx_total + zone + dist_x + dist_z +
                     dist_prop + I(dist_x^2) + I(dist_z^2) + release_spin_rate + pred_bstrike,
                   data = ch_l, family = binomial)


ch_l_strike_formula <- step(ch_l_strike, direction = "backward")
ch_l_strike_formula$formula

# Removed: pfx_x, pfx_z 

# Model Evaluation
# summary(ch_r_strike)

ch_l_strike_test <- augment(ch_l_strike, type.predict = "response") %>% 
  rename(prediction = `.fitted`) %>%
  mutate(Response = "strike",
         Hand = "left",
         pitch = "changeup")


rmse(ch_l_strike_test$prediction, ch_l_strike_test$is_strike)


models <- rbind(
  tidy(fb_r_whiff) %>% mutate(Response = "whiff",
                              Hand = "right", Pitch = "fastball"),
  tidy(fb_r_barrel) %>% mutate(Response = "barrel",
                               Hand = "right", Pitch = "fastball"),
  tidy(fb_r_strike) %>% mutate(Response = "strike",
                               Hand = "right", Pitch = "fastball"),
  
  tidy(fb_l_whiff) %>% mutate(Response = "whiff",
                              Hand = "left", Pitch = "fastball"),
  tidy(fb_l_barrel) %>% mutate(Response = "barrel",
                               Hand = "left", Pitch = "fastball"),
  tidy(fb_l_strike) %>% mutate(Response = "strike",
                               Hand = "left", Pitch = "fastball"),
  
  tidy(sl_r_whiff) %>% mutate(Response = "whiff",
                              Hand = "right", Pitch = "slider"),
  tidy(sl_r_barrel) %>% mutate(Response = "barrel",
                               Hand = "right", Pitch = "slider"),
  tidy(sl_r_strike) %>% mutate(Response = "strike",
                               Hand = "right", Pitch = "slider"),
  
  tidy(sl_l_whiff) %>% mutate(Response = "whiff",
                              Hand = "left", Pitch = "slider"),
  tidy(sl_l_barrel) %>% mutate(Response = "barrel",
                               Hand = "left", Pitch = "slider"),
  tidy(sl_l_strike) %>% mutate(Response = "strike",
                               Hand = "left", Pitch = "slider"),
  
  tidy(cu_r_whiff) %>% mutate(Response = "whiff",
                              Hand = "right", Pitch = "curveball"),
  tidy(cu_r_barrel) %>% mutate(Response = "barrel",
                               Hand = "right", Pitch = "curveball"),
  tidy(cu_r_strike) %>% mutate(Response = "strike",
                               Hand = "right", Pitch = "curveball"),
  
  tidy(cu_l_whiff) %>% mutate(Response = "whiff",
                              Hand = "left", Pitch = "curveball"),
  tidy(cu_l_barrel) %>% mutate(Response = "barrel",
                               Hand = "left", Pitch = "curveball"),
  tidy(cu_l_strike) %>% mutate(Response = "strike",
                               Hand = "left", Pitch = "curveball"),
  
  tidy(ch_r_whiff) %>% mutate(Response = "whiff",
                              Hand = "right", Pitch = "changeup"),
  tidy(ch_r_barrel) %>% mutate(Response = "barrel",
                               Hand = "right", Pitch = "changeup"),
  tidy(ch_r_strike) %>% mutate(Response = "strike",
                               Hand = "right", Pitch = "changeup"),
  
  tidy(ch_l_whiff) %>% mutate(Response = "whiff",
                              Hand = "left", Pitch = "changeup"),
  tidy(ch_l_barrel) %>% mutate(Response = "barrel",
                               Hand = "left", Pitch = "changeup"),
  tidy(ch_l_strike) %>% mutate(Response = "strike",
                               Hand = "left", Pitch = "changeup")
  
)


models <- models %>% 
  filter(!(term  %in%  c("(Intercept)", "pred_bwhiff", "pred_bbarrel", "pred_bstrike")))


# Use slice_min to find zone with lowest p-value

zone_terms <- models %>%
  filter(str_detect(term, "zone")) %>%
  slice_min(`p.value`, by = c(Response, Hand, Pitch)) %>%
  mutate(term = "best_zone")


# Combine full data with new best_zone

models <- models %>%
  filter(!str_detect(term, "zone")) %>%
  rbind(zone_terms)
