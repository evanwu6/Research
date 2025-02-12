### Code to simplify best_zone creation with models3.csv file

# Required libraries

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)


# Read in data and remove unnecessary columns

model <- read_csv("models3.csv") %>% 
  select(-...1) %>% 
  filter(!(term  %in%  c("(Intercept)", "pred_bwhiff", "pred_bbarrel", "pred_bstrike")))


# Use slice_min to find zone with lowest p-value

zone_terms <- model %>%
  filter(str_detect(term, "zone")) %>%
  slice_min(`p.value`, by = c(Response, Hand, Pitch)) %>%
  mutate(term = "best_zone")


# Combine full data with new best_zone

model <- model %>%
  filter(!str_detect(term, "zone")) %>%
  rbind(zone_terms)


# Model heat map (compare responses within batter hand / pitch type combinations)

model %>% 
  mutate(log_pval = -log10(p.value)) %>% 
  ggplot(aes(x = Response, y = term,
             fill = log_pval)) +
  scale_fill_gradient2(low = "khaki1", high = "green", na.value = "darkgreen") +
  geom_tile() +
  facet_grid(Hand ~ Pitch) +
  theme_dark()


# Model heat map (compare pitch types within batter hand / response combinations)

model %>% 
  mutate(log_pval = -log10(p.value)) %>% 
  ggplot(aes(x = Pitch, y = term,
             fill = log_pval)) +
  scale_fill_gradient2(low = "khaki1", high = "green", na.value = "darkgreen") +
  geom_tile() +
  facet_grid(Hand ~ Response) +
  theme_dark()




#####



# Take rhp data from Week 10.Rmd and convert responses to 0/1, remove NAs

rhp <- rhp %>%
  mutate(Barrel = ifelse(Barrel == TRUE, 1, 0),
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
    for(response in c("whiff", "Barrel", "STRIKE")) {
      
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
      if(response == "Barrel") { data <- rename(data, response = Barrel,
                                        pred = pred_bbarrel) }
      if(response == "STRIKE") { data <- rename(data, response = STRIKE,
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

