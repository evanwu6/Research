pitchers %>% 
  filter(pitch_type == "FF") %>% 
  left_join(avg, by = c("player_name", "pitch_type")) %>%
  mutate(delta_speed = pitch_speed - speed,
         delta_mov_x = pfx_x - `_x`,
         delta_mov_z = pfx_x - `_z`,
         delta_spin = release_spin_rate - spin_rate) %>%
  filter(!is.na(estimated_woba_using_speedangle)) %>% 
  # mutate(player_name = fct_reorder(as.factor(player_name), ID, .desc = TRUE)) %>%
  ggplot(aes(group = player_name, x = "", y = delta_speed, color = estimated_woba_using_speedangle)) +
  geom_boxplot(aes(fill = ID), color = "black") +
  # geom_jitter(width = 0.075, height = 0, alpha = 0.075) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0),
             alpha = 0.5) +
  scale_color_gradient2(low = "green", midpoint = 0.41, high = "red") +
  facet_wrap(vars(ID), labeller = ) +
  labs(title = "Speed Differential (Against Average) for Fastballs") +
  NULL

delt <- pitchers %>% 
  filter(pitch_type == "FF") %>% 
  left_join(avg, by = c("player_name", "pitch_type")) %>%
  mutate(delta_speed = pitch_speed - speed,
         delta_mov_x = pfx_x - `_x`,
         delta_mov_z = pfx_x - `_z`,
         delta_spin = release_spin_rate - spin_rate)

delt %>% 
  ggplot(aes(x = delta_speed, y = estimated_woba_using_speedangle)) +
  geom_point(aes(color = ID), alpha = 0.2) +
  facet_wrap(~ player_name) +
  geom_smooth(se = FALSE)

