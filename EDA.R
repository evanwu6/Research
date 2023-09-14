library(tidyverse)
library(readxl)

data <- read_csv("pitch_arsenals.csv")
movement <- read_csv("~/Downloads/Baseball Analytics/Diamond Dollars 2023/Statcast Data 2022/Group B/Libka 9-12-22/TEX at MIA 9-12-22.csv")

movement %>% 
  select(pitch_type, release_speed, p_throws, pfx_x, pfx_z) %>% 
  View()

strider <- read_csv("Strider_9_13_23.csv") %>% 
  select(-game_date, -`pitcher...8`, -spin_dir:-break_length_deprecated,
         -game_type, -home_team, -away_team)

median(data$ff_avg_spin, na.rm = TRUE)
median(data$sl_avg_spin, na.rm = TRUE)
median(data$cu_avg_spin, na.rm = TRUE)

strider %>% 
  ggplot(aes(x = -plate_x, y = plate_z)) +
  geom_segment(x = -8.5/12, y = 18/12, xend = 8.5/12, yend = 18/12,
               linewidth = 0.5) +
  geom_segment(x = -8.5/12, y = 44/12, xend = 8.5/12, yend = 44/12,
               linewidth = 0.5) +
  geom_segment(x = -8.5/12, y = 18/12, xend = -8.5/12, yend = 44/12,
               linewidth = 0.5) +
  geom_segment(x = 8.5/12, y = 18/12, xend = 8.5/12, yend = 44/12,
               linewidth = 0.5) +
  geom_point(aes(shape = pitch_type, color = pitch_type), alpha = 0.85) + 
  coord_fixed() +
  labs(color = "Pitch",
       shape = "Pitch",
       title = "Spencer Strider",
       subtitle = "Sep. 13, 2023 vs. PHI",
       caption = "Data from Baseball Savant") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


strider %>% 
  ggplot(aes(x = -pfx_x*12, y = pfx_z*12)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  geom_hline(yintercept = 0, linewidth= 0.5) +
  geom_point(aes(shape = pitch_type, color = pitch_type), alpha = 0.85) + 
  coord_fixed() +
  labs(x = "Induced Horizontal Movement",
       y = "Induced Vertical Movement",
       color = "Pitch",
       shape = "Pitch",
       title = "Spencer Strider",
       subtitle = "Sep. 13, 2023 vs. PHI",
       caption = "Data from Baseball Savant") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
