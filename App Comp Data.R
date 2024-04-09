library(tidyverse)

# Name Function ####
swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}

# Reading in Movement Data ####
movement_2021 <- read_csv("App Comp Data/movement_2021.csv") %>% 
  rename(pitcher_name = `last_name, first_name`) %>% 
  select(pitcher_name, pitcher_id, pitch_hand, year, pitch_type, 
         pitches_thrown, total_pitches, 
         avg_speed, pitcher_break_x, pitcher_break_z)

movement_2022 <- read_csv("App Comp Data/movement_2022.csv") %>% 
  rename(pitcher_name = `last_name, first_name`) %>% 
  select(pitcher_name, pitcher_id, pitch_hand, year, pitch_type, 
         pitches_thrown, total_pitches, 
         avg_speed, pitcher_break_x, pitcher_break_z)

movement_2023 <- read_csv("App Comp Data/movement_2023.csv") %>% 
  rename(pitcher_name = `last_name, first_name`) %>% 
  select(pitcher_name, pitcher_id, pitch_hand, year, pitch_type, 
         pitches_thrown, total_pitches, 
         avg_speed, pitcher_break_x, pitcher_break_z)


# Reading in Spin Data ####
arsenal_2021 <- read_csv("App Comp Data/arsenal_2021.csv") %>% 
  pivot_longer(cols = ff_avg_spin:sv_avg_spin,
               names_to = "pitch_type",
               values_to = "avg_spin") %>% 
  mutate(pitch_type = substr(pitch_type, 1, 2),
         pitch_type = toupper(pitch_type)) %>% 
  filter(!is.na(`avg_spin`)) %>% 
  rename(pitcher_id = pitcher) %>% 
  select(-`last_name, first_name`) %>% 
  mutate(year = 2021)

arsenal_2022 <- read_csv("App Comp Data/arsenal_2022.csv") %>% 
  pivot_longer(cols = ff_avg_spin:sv_avg_spin,
               names_to = "pitch_type",
               values_to = "avg_spin") %>% 
  mutate(pitch_type = substr(pitch_type, 1, 2),
         pitch_type = toupper(pitch_type)) %>% 
  filter(!is.na(`avg_spin`)) %>% 
  rename(pitcher_id = pitcher) %>% 
  select(-`last_name, first_name`) %>% 
  mutate(year = 2022)

arsenal_2023 <- read_csv("App Comp Data/arsenal_2023.csv") %>% 
  pivot_longer(cols = ff_avg_spin:sv_avg_spin,
               names_to = "pitch_type",
               values_to = "avg_spin") %>% 
  mutate(pitch_type = substr(pitch_type, 1, 2),
         pitch_type = toupper(pitch_type)) %>% 
  filter(!is.na(`avg_spin`)) %>% 
  rename(pitcher_id = pitcher) %>% 
  select(-`last_name, first_name`) %>% 
  mutate(year = 2023)
  

# Combine Data ####
data2021 <- movement_2021 %>% 
  left_join(arsenal_2021, by = c("pitcher_id", "pitch_type", "year")) %>% 
  mutate(pitcher_name = sapply(pitcher_name, swap_names)) %>% 
  select(-pitcher_id)

data2022 <- movement_2022 %>% 
  left_join(arsenal_2022, by = c("pitcher_id", "pitch_type", "year")) %>% 
  mutate(pitcher_name = sapply(pitcher_name, swap_names)) %>% 
  select(-pitcher_id)

data2023 <- movement_2023 %>% 
  left_join(arsenal_2023, by = c("pitcher_id", "pitch_type", "year")) %>% 
  mutate(pitcher_name = sapply(pitcher_name, swap_names)) %>% 
  select(-pitcher_id)

data <- rbind(data2021, data2022, data2023) %>% 
  na.omit()

write.csv(data, "Pitcher_Comps.csv")
