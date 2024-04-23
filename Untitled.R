# Refined model

Response <- "Barrel"
P_Hand <- "right"
B_Hand <- "right"
pitch <- "fastball"


# Refined Model

fb_r_barrel <- glm(Barrel ~ pred_bbarrel +
                     pitch_speed + pfx_z + dist_x +
                     dist_z + dist_prop + zone + I(dist_x^2) + I(dist_z^2), 
                   data = fb_r, family = binomial)

fb_r_barrel_final <- step(fb_r_barrel)

fb_r_barrel_test <- augment(fb_r_barrel, type.predict = "response") %>%
  rename(prediction = `.fitted`) %>% 
  mutate(Response = Response,
         Hand = Hand,
         pitch = pitch)


tidy(fb_r_barrel_final) %>%
  mutate(Response = Response,
         P_Hand = P_Hand,
         B_Hand = B_Hand,
         pitch = pitch)

tidy(fb_r_barrel_final)$term
