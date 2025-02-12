# Libraries ####
library(tidyverse)


# Pitchers Dataset ####
files <- list.files("Pitch Level Data/Pitcher Comps 2022")

pitchers <- read_csv(paste0("Pitch Level Data/Pitcher Comps 2022/", files[1]))

for(i in 2:length(files)) {
  pitchers <- rbind(pitchers,
                    read_csv(paste0("Pitch Level Data/Pitcher Comps 2022/",files[i])))
}

# Strike / Ball
pitchers <- pitchers %>% 
  mutate(zone = ifelse(plate_z >= (sz_bot - 1.45/12) &
                         plate_z <= (sz_top + 1.45/12) &
                         abs(plate_x) <= (17+1.45)/12/2 ,
                       "1", "0"))

# Distance from Center
pitchers <- pitchers %>% 
  filter(pitch_type != "Pitch Out",
         !is.na(pitch_type)) %>% 
  mutate(
    loc_x = abs(plate_x),
    loc_y = abs(plate_z - (sz_top + sz_bot) / 2),
    dist = sqrt(loc_x^2 + loc_y^2))

# Speed Change
pitchers <- pitchers %>% 
  mutate(speed_change = release_speed - mean(release_speed), 
         .by = c(pitch_type, game_date, pitcher...8) )

# Break Change
pitchers <- pitchers %>% 
  mutate(pfx_total = sqrt(pfx_x^2 + pfx_z^2)) %>% 
  mutate(break_change = pfx_total - mean(pfx_total), 
         .by = c(pitch_type, game_date, pitcher...8) )

# Good, Decent, Bad ID
pitchers <- pitchers %>% 
  mutate(ID = case_when(
    player_name  %in% c("Gallen, Zac", "Manoah, Alek", "Scherzer, Max") ~ "Great",
    player_name  %in% c("López, Pablo", "Garcia, Luis", "Taillon, Jameson") ~ "Decent",
    player_name  %in% c("Berríos, José", "Gray, Josiah", "Keller, Brad") ~ "Bad"
  ))

# Trimming CSV
pitchers <- pitchers %>% 
  select(-spin_dir:-break_length_deprecated,
         -vx0:-az,
         -fielder_2...61:-fielder_9)

# Writing CSV
write.csv(pitchers, "CSVs/pitcher_comps.csv", row.names = FALSE)


