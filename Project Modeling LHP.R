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

lhp <- all_pitches %>% 
  filter(p_throws == "L") %>% 
  filter(!is.na(pfx_x), !is.na(pfx_z), !is.na(pitch_speed), !is.na(release_spin_rate), 
         !is.na(pfx_total))

# Batter Whiff
batter_stats <- lhp %>% 
  summarize(rate = mean(whiff),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_whiff = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bwhiff = (pitches / 300)*rate + ((300-pitches)/300)*pitch_whiff) %>% 
  mutate(pred_bwhiff = ifelse(pitches >= 300, rate, pred_bwhiff))


# Merging Whiff Prediction with lhp
lhp <- lhp %>% 
  left_join(select(batter_stats, batter, pitch_type, pred_bwhiff), 
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
batter_stats2 <- lhp %>% 
  summarize(rate = mean(is_barrel),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_barrel = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bbarrel = (pitches / 300)*rate + ((300-pitches)/300)*pitch_barrel) %>% 
  mutate(pred_bbarrel = ifelse(pitches >= 300, rate, pred_bbarrel))

lhp <- lhp %>% 
  left_join(select(batter_stats2, batter, pitch_type, pred_bbarrel), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))


# Benefit
batter_stats3 <- lhp %>% 
  summarize(rate = mean(is_benefit),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  filter(!is.na(rate)) %>% 
  mutate(pitch_benefit = weighted.mean(rate, pitches),
         .by = pitch_type) %>% 
  mutate(pred_bbenefit = (pitches / 300)*rate + ((300-pitches)/300)*pitch_benefit) %>%
  mutate(pred_bbenefit = ifelse(pitches >= 300, rate, pred_bbenefit))

lhp <- lhp %>% 
  left_join(select(batter_stats3, batter, pitch_type, pred_bbenefit), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type")) %>% 
  filter(!is.na(pred_bbenefit))







# Convert responses to 0/1, remove NAs

lhp <- lhp %>%
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

# colSums(is.na(lhp)) %>% View

index <- 0

for(pitch in c("FF", "SL", "CH", "CU", "FC", "SI")) {
  for(bhand in c("R", "L")) {
    for(response in c("whiff", "is_barrel", "is_benefit")) {
      
      index <- index + 1
      
      data <- lhp %>%
        filter(pitch_type == pitch,
               hitter == bhand) %>% 
        mutate(pfx_x = pfx_x*12,
               pfx_z = pfx_z*12,
               pfx_total = sqrt(pfx_x^2 + pfx_z^2)) %>% 
        mutate(dist_x = -dist_x,
               zone = factor(zone, levels = c(5, 1:4, 6:9, 11:14)))
      
      if(response == "whiff") { data <- rename(data, response = whiff,
                                               pred = pred_bwhiff) }
      if(response == "is_barrel") { data <- rename(data, response = is_barrel,
                                                pred = pred_bbarrel) }
      if(response == "is_benefit") { data <- rename(data, response = is_benefit,
                                                pred = pred_bbenefit) }
      
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
      
      print(paste("Model", index, "of", 6*2*3, "completed"))
    }
  }
}

results %>% 
  mutate(Response = case_when(Response == "whiff" ~ "whiff",
                              Response == "is_barrel" ~ "barrel",
                              Response == "is_benefit" ~ "benefit")) %>% 
write.csv("models_lhp.csv", row.names = FALSE)


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

