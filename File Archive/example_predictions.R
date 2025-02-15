library(tidyverse)
library(tidyr)
options(scipen = 999)

models <- read_csv("models4.csv") %>% 
  mutate(Pitch = str_replace(Pitch, "FF", "fastball"),
         Pitch = str_replace(Pitch, "SL", "slider"),
         Pitch = str_replace(Pitch, "CU", "curveball"),
         Pitch = str_replace(Pitch, "CH", "changeup")) %>% 
  mutate(Hand = ifelse(BHand == "R", "right", "left")) %>% 
  mutate(Response = case_when(Response == "STRIKE" ~ "strike",
                              Response == "Barrel" ~ "barrel",
                              Response == "whiff" ~ "whiff")) %>% 
  mutate(term = case_when(term == "pred" & Response == "whiff" ~ "pred_bwhiff",
                          term == "pred" & Response == "barrel" ~ "pred_bbarrel",
                          term == "pred" & Response == "strike" ~ "pred_bstrike",
                          TRUE ~ term))

zones <- models %>%
  select(zone1:zone9) %>%
  

table(models$term)
table(models$Pitch)
table(models$Hand)

models %>%
  filter(Hand == "right",
         Response == "whiff") %>%
  ggplot(aes(x = Pitch, y = term, fill = abs(log10(p.value)))) +
  geom_tile()


models %>%
  filter(Hand == "right",
         Response == "whiff") %>%
  filter(term == "pred_bwhiff") %>%
  pull(p.value) %>%
  log10()
  View
  
models %>%
    filter(Hand == "right") %>%
  filter(term != "pred_bbarrel", term != "(Intercept)") %>%
  ggplot(aes(x = Pitch, y = term, fill = abs(log10(p.value)))) +
  geom_tile() +
  facet_wrap(vars(Response))



test_data <- models %>%
  filter(Hand == "right",
         Pitch == "slider",
         Response == "whiff")


zone_data <- data.frame(x = seq(-2, 2, .1),
                        z = seq(-2.5, 1.5, .1),
                        response = rep(c("whiff", "strike", "barrel"), length.out = 41),
                        hand = rep(c("right", "left"), length.out = 41),
                        pitch = rep(c("fastball", "slider", "curveball", "changeup"), length.out = 41)) %>%
  expand(x, z, response, hand, pitch) %>%
  mutate(pred = NA)

for(response in c("whiff", "strike", "barrel")) {
  for(hand in c("right", "left")) {
    for(pitch in c("fastball", "slider", "curveball", "changeup")) {
      test_data <- filter(models, Hand == hand, Pitch == pitch, Response == response)
      test_x <- pull(filter(test_data, term == "dist_x"), estimate)
      test_x2 <- pull(filter(test_data, term == "I(dist_x^2)"), estimate)
      test_z <- pull(filter(test_data, term == "dist_z"), estimate)
      test_z2 <- pull(filter(test_data, term == "I(dist_z^2)"), estimate)
      test_prop <- pull(filter(test_data, term == "dist_prop"), estimate)
      
      if(is_empty(test_x)) {test_x <- 0}
      if(is_empty(test_z)) {test_z <- 0}
      if(is_empty(test_x2)) {test_x2 <- 0}
      if(is_empty(test_z2)) {test_z2 <- 0}
      if(is_empty(test_prop)) {test_prop <- 0}
      # if(!is.null(is.na(test_prop))) {test_prop <- 0}
      
      zone_data[zone_data$response == response & zone_data$hand == hand & zone_data$pitch == pitch,] <- zone_data[zone_data$response == response & zone_data$hand == hand & zone_data$pitch == pitch,] %>%
        mutate(pred = x * test_x + z * test_z +
                 x^2 * test_x2 + z^2 * test_z2 +
                 sqrt(x^2 + z^2) * test_prop)
    }
  }
}




zone_data %>%
  mutate(pred = 1 / (1 + exp(-pred))) %>%
  filter(response == "whiff") %>%
  ggplot(aes(x = x, y = z, color = pred)) +
  geom_point(size = 2, alpha = .5) +
  coord_fixed() +
  facet_grid(pitch ~ hand)


write.csv(zone_data, "zone_data.csv")


