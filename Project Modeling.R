library(tidyverse)
library(knitr)
library(glmnet)
library(patchwork)
library(olsrr)
library(broom)
library(RColorBrewer)
library(Metrics)
library(stringr)

options(scipen = 999)


# Geom Zone (Jackie's) ####

geom_zone <- function(top = 11/3, bottom = 3/2, linecolor = "black"){
  geom_rect(xmin = -.7083, xmax = .7083, ymin = bottom, ymax = top,
            alpha = 0, color = linecolor, linewidth = 0.75)
}

# c(0, 0, -.25, -.5, -.25))

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

swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}


#### Data

all_pitches <- read_csv("CSVs/all_pitches.csv")

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




# Take rhp data from Week 10.Rmd and convert responses to 0/1, remove NAs

rhp <- rhp %>%
  mutate(Barrel = ifelse(is_barrel == TRUE, 1, 0),
         whiff = ifelse(whiff == TRUE, 1, 0)) %>%
  filter(!is.na(pitch_speed),
         !is.na(release_spin_rate))


# Create data frame for model results

results <- data.frame(term = NULL, 
                      estimate = NULL, 
                      std.error = NULL, 
                      statistic = NULL, 
                      p.value = NULL,
                      Response = NULL,
                      BHand = NULL,
                      Pitch = NULL)


# Loop through models using backward elimination

index <- 0

for(pitch in c("FF", "SL", "CH", "CU")) {
  for(bhand in c("R", "L")) {
    for(response in c("whiff", "is_barrel", "is_strike")) {
      
      index <- index + 1
      
      data <- rhp %>%
        filter(pitch_type == pitch,
               hitter == bhand) %>% 
        mutate(pfx_x = pfx_x*12,
               pfx_z = pfx_z*12,
               pfx_total = sqrt(pfx_x^2 + pfx_z^2)) %>% 
        mutate(dist_x = -dist_x)
      
      if(response == "whiff") { data <- rename(data, response = whiff,
                                               pred = pred_bwhiff) }
      if(response == "is_barrel") { data <- rename(data, response = is_barrel,
                                                pred = pred_bbarrel) }
      if(response == "is_strike") { data <- rename(data, response = is_strike,
                                                pred = pred_bstrike) }
      
      model <- glm(response ~ pitch_speed + pfx_x + pfx_z + pfx_total + zone + 
                     dist_x + dist_z + dist_prop + I(dist_x^2) + I(dist_z^2) + 
                     release_spin_rate + pred,
                   data = data, family = binomial)
      
      model2 <- step(model, direction = "backward") %>%
        tidy() %>%
        mutate(Response = response,
               BHand = bhand,
               Pitch = pitch)
      
      if(nrow(results > 0)) { results <- rbind(results, model2) }
      if(nrow(results) == 0) { results <- model2 }
      
      print(paste("Model", index, "of", 24, "completed"))
    }
  }
}


write.csv(results, "models4.csv", row.names = FALSE)


# Graph the results 
# (Note: These differ from results above. Automated method uses AIC instead of
# p-values for elimination.)

# Determine best zone for results

zone_terms <- results %>%
  filter(str_detect(term, "zone")) %>%
  slice_min(`p.value`, by = c(Response, BHand, Pitch)) %>%
  mutate(term = "best_zone")

model <- results %>%
  filter(!str_detect(term, "zone"),
         term != "(Intercept)", 
         term != "pred") %>%
  rbind(zone_terms)


# Model heat map (compare responses within batter hand / pitch type combinations)

model %>% 
  mutate(log_pval = -log10(p.value)) %>% 
  ggplot(aes(x = Response, 
             y = term,
             fill = log_pval)) +
  scale_fill_gradient2(low = "khaki1", 
                       high = "green", 
                       na.value = "darkgreen") +
  geom_tile(color = "darkgray") +
  facet_grid(BHand ~ Pitch) +
  theme_dark()


# Model heat map (compare pitch_type within batter hand / response combinations)

model %>% 
  mutate(log_pval = -log10(p.value)) %>% 
  ggplot(aes(x = Pitch, 
             y = term,
             fill = log_pval)) +
  scale_fill_gradient2(low = "khaki1", 
                       high = "green", 
                       na.value = "darkgreen") +
  geom_tile(color = "darkgray") +
  facet_grid(BHand ~ Response) +
  theme_dark()




