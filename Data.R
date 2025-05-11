# Libraries ####
library(tidyverse)
library(readxl)
library(ggforce)
library(knitr)
library(forcats)


# Barrel Function
is.barrel <- function(LA, EV){
  upper <- 1.11*EV - 78.89
  lower <- -EV + 124
  outcome <- (LA >= lower) & (LA <= upper) & (EV >= 98) & (LA >= 8) & (LA <= 50)
  outcome <- replace_na(outcome, FALSE)
  outcome
}

# Name Function ####
swap_names <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) == 2) {
    return(paste(rev(parts), collapse = " "))
  } else {
    return(name)
  }
}

# 2022 Seasonal Data ####

# Overall Numbers for Pitchers
# URL: https://baseballsavant.mlb.com/leaderboard/custom?year=2022&type=pitcher&filter=&sort=4&sortDir=asc&min=100&selections=p_game,p_formatted_ip,pa,hit,home_run,strikeout,walk,k_percent,bb_percent,batting_avg,slg_percent,on_base_percent,on_base_plus_slg,babip,p_earned_run,p_win,p_loss,p_era,xba,xslg,woba,xwoba,xobp,wobacon,xwobacon,exit_velocity_avg,launch_angle_avg,sweet_spot_percent,barrel_batted_rate,&chart=false&x=xba&y=xba&r=no&chartType=beeswarm
# Downloaded: September 29, 2023
stats_2022 <- read_csv("Pitcher Seasonal Data/2022_stats.csv")

stats_2022 <- stats_2022 %>% 
  rename(Name = `last_name, first_name`,
         IP = p_formatted_ip,
         G = p_game,
         AVG = batting_avg,
         SLG = slg_percent,
         OBP = on_base_percent,
         OPS = on_base_plus_slg,
         ER = p_earned_run,
         wins = p_win,
         losses = p_loss,
         ERA = p_era,
         ev_avg = exit_velocity_avg,
         la_avg = launch_angle_avg,
         PA = pa,
         HR = home_run,
         H = hit,
         K = strikeout,
         BB = walk) %>% 
  select(-...33)

# 2022 Pitch Arsenal Data ####
# Downloaded: September 23, 2023

# Pitch Speed
# URL: https://baseballsavant.mlb.com/leaderboard/pitch-arsenals?year=2022&min=100&type=avg_speed&hand=
arsenal_speed <- read_csv("Pitch Type Data/2022_arsenal_speed.csv")
  
arsenal_speed <- arsenal_speed %>% 
  pivot_longer(cols = ff_avg_speed:sv_avg_speed, 
               names_to = "pitch_type",
               values_to = "pitch_speed") %>% 
  filter(!is.na(pitch_speed)) %>% 
  mutate(pitch_type = str_replace(pitch_type, "ff_avg_speed", "FF"),
         pitch_type = str_replace(pitch_type, "si_avg_speed", "SI"),
         pitch_type = str_replace(pitch_type, "fc_avg_speed", "FC"),
         pitch_type = str_replace(pitch_type, "sl_avg_speed", "SL"),
         pitch_type = str_replace(pitch_type, "ch_avg_speed", "CH"),
         pitch_type = str_replace(pitch_type, "cu_avg_speed", "CU"),
         pitch_type = str_replace(pitch_type, "fs_avg_speed", "FS"),
         pitch_type = str_replace(pitch_type, "st_avg_speed", "ST"),
         pitch_type = str_replace(pitch_type, "sv_avg_speed", "SV"))


# Pitch Spin
# URL: https://baseballsavant.mlb.com/leaderboard/pitch-arsenals?year=2022&min=100&type=avg_spin&hand=
arsenal_spin <- read_csv("Pitch Type Data/2022_arsenal_spin.csv")

arsenal_spin <- arsenal_spin %>% 
  pivot_longer(cols = ff_avg_spin:sv_avg_spin, 
               names_to = "pitch_type",
               values_to = "pitch_spin") %>% 
  filter(!is.na(pitch_spin)) %>% 
  mutate(pitch_type = str_replace(pitch_type, "ff_avg_spin", "FF"),
         pitch_type = str_replace(pitch_type, "si_avg_spin", "SI"),
         pitch_type = str_replace(pitch_type, "fc_avg_spin", "FC"),
         pitch_type = str_replace(pitch_type, "sl_avg_spin", "SL"),
         pitch_type = str_replace(pitch_type, "ch_avg_spin", "CH"),
         pitch_type = str_replace(pitch_type, "cu_avg_spin", "CU"),
         pitch_type = str_replace(pitch_type, "fs_avg_spin", "FS"),
         pitch_type = str_replace(pitch_type, "st_avg_spin", "ST"),
         pitch_type = str_replace(pitch_type, "sv_avg_spin", "SV"))


# Pitch Usage (Percentage)
# URL: https://baseballsavant.mlb.com/leaderboard/pitch-arsenals?year=2022&min=100&type=n_&hand=
arsenal_perc <- read_csv("Pitch Type Data/2022_arsenal_perc.csv")

arsenal_perc <- arsenal_perc %>% 
  pivot_longer(cols = n_ff:n_sv, 
               names_to = "pitch_type",
               values_to = "pitch_usage") %>% 
  filter(!is.na(pitch_usage)) %>% 
  mutate(pitch_type = str_replace(pitch_type, "n_ff", "FF"),
         pitch_type = str_replace(pitch_type, "n_si", "SI"),
         pitch_type = str_replace(pitch_type, "n_fc", "FC"),
         pitch_type = str_replace(pitch_type, "n_sl", "SL"),
         pitch_type = str_replace(pitch_type, "n_ch", "CH"),
         pitch_type = str_replace(pitch_type, "n_cu", "CU"),
         pitch_type = str_replace(pitch_type, "n_fs", "FS"),
         pitch_type = str_replace(pitch_type, "n_st", "ST"),
         pitch_type = str_replace(pitch_type, "n_sv", "SV"))


# Pitch Movement
# URL: https://baseballsavant.mlb.com/leaderboard/pitch-movement?year=2022&team=&min=50&pitch_type=ALL&hand=&x=diff_x_hidden&z=diff_z_hidden
arsenal_movement <- read_csv("Pitch Type Data/2022_arsenal_movement.csv") %>% 
  mutate(pitch_type = str_replace(pitch_type, "SIFT", "SI"),
         pitch_type = str_replace(pitch_type, "CUKC", "CU")) %>% 
  filter(pitch_type != "FA")

arsenal_movement <- arsenal_movement %>% 
  rename(pitch_name = pitch_type_name,
         player_id = pitcher_id) %>% 
  select(player_id, pitch_type, pitch_hand,
         pitcher_break_x, pitcher_break_z, avg_speed)


# Pitch Stats
# URL: https://baseballsavant.mlb.com/leaderboard/pitch-arsenal-stats?type=pitcher&pitchType=&year=2022&team=&min=10
arsenal_stats <- read_csv("Pitch Type Data/2022_arsenal_stats.csv") %>% 
  mutate(pitch_type = str_replace(pitch_type, "SIFT", "SI"),
         pitch_type = str_replace(pitch_type, "CUKC", "CU")) %>% 
  rename(rv100 = run_value_per_100,
         team = team_name_alt) %>% 
  filter(pitch_type != "FA")

# Combining Databases
arsenal <- arsenal_stats %>% 
  left_join(select(arsenal_speed, pitcher:pitch_speed), 
            by = c("player_id" = "pitcher", "pitch_type" = "pitch_type")) %>% 
  left_join(select(arsenal_spin, pitcher:pitch_spin), 
            by = c("player_id" = "pitcher", "pitch_type" = "pitch_type")) %>% 
  left_join(arsenal_movement, 
            by = c("player_id" = "player_id", "pitch_type" = "pitch_type")) %>% 
  select(first_name, last_name, player_id, pitch_hand,
         team:pitch_name, pitches, pitch_usage,
         pitch_speed, pitch_spin, pitcher_break_x, pitcher_break_z,
         rv100, run_value,
         pa:hard_hit_percent) %>% 
  filter(!is.na(pitch_speed)) %>% 
  filter(!is.na(pitcher_break_z))

arsenal <- arsenal %>% 
  rename(xBA = est_ba,
         xSLG = est_slg,
         xwOBA = est_woba,
         PA = pa,
         AVG = ba,
         SLG = slg,
         wOBA = woba,
         spin_rate = pitch_spin)


# Pitch-by-Pitch Data (9 Pitchers) ####
files <- list.files("Pitch Level Data/Pitcher Comps 2022/")

pitchers <- read_csv(paste0("Pitch Level Data/Pitcher Comps 2022/", files[1]))

for(i in 2:length(files)) {
  pitchers <- rbind(pitchers,
                    read_csv(paste0("Pitch Level Data/Pitcher Comps 2022/",files[i])))
}

add <- stats_2022 %>% 
  select(player_id, woba)

pitchers <- pitchers %>% 
  left_join(add, by = c("pitcher...8" = "player_id")) %>% 
  mutate(ID = case_when(woba > 0.330 ~ "Bad",
                        woba < 0.280 ~ "Great",
                        TRUE ~ "Decent"),
         win_exp_added = case_when(inning_topbot == "Bot"  ~ -delta_home_win_exp,
                                   inning_topbot == "Top"  ~ delta_home_win_exp),
         run_exp_added = -delta_run_exp,
         ra_on_play = post_bat_score - bat_score) %>% 
  rename(player_id = pitcher...8,
         hitter = stand,
         pitch_speed = release_speed) %>% 
  select(ID, player_name, player_id, game_date, pitch_type, pitch_name,
         pitch_speed, spin_axis, pfx_x:plate_z, ra_on_play,
         release_pos_x, release_pos_z, bb_type:strikes, pitch_number,
         hitter, home_team:type, outs_when_up, inning, ra_on_play,
         run_exp_added, win_exp_added,
         sz_top, sz_bot, hit_distance_sc:release_extension,
         estimated_ba_using_speedangle:woba_value, at_bat_number, 
         description, events, des, on_3b:on_1b, outs_when_up, zone) %>% 
  mutate(pitch_name = str_replace(pitch_name, "4-Seam Fastball", "4-Seam")) 


# Making Pitcher Comparions ####
comps_2022 <- stats_2022 %>% 
  left_join(filter(arsenal_speed, pitch_type == "FF"), c("player_id" = "pitcher")) %>% 
  select(first_name, last_name, player_id, woba, pitch_speed) %>% 
  rename(FF_speed = pitch_speed) %>% 
  arrange(woba)

p10 <- quantile(comps_2022$woba, probs = 0.9)
p50 <- quantile(comps_2022$woba, probs = 0.5)
p90 <- quantile(comps_2022$woba, probs = 0.1)

pitchers_10 <- comps_2022 %>% 
  filter(woba > p10 - 0.010 & woba < p10 + 0.010,
         FF_speed > 93 & FF_speed < 95) %>% 
  mutate(ID = "Bad")

pitchers_50 <- comps_2022 %>% 
  filter(woba > p50 - 0.010 & woba < p50 + 0.010,
         FF_speed > 93 & FF_speed < 95) %>% 
  mutate(ID = "Decent")

pitchers_90 <- comps_2022 %>% 
  filter(woba > p90 - 0.010 & woba < p90 + 0.010,
         FF_speed > 93.5 & FF_speed < 94.5) %>% 
  mutate(ID = "Great")

candidates <- rbind(pitchers_10, pitchers_50, pitchers_90) 

sample <- candidates %>% 
  filter(player_id %in% c("641745", "621244", "680686",
                          "677651", "641154", "592791",
                          "668678", "453286", "666201"))


# Bases Empty Dataset ####

files <- list.files("Pitch Level Data/Pitcher Comps 2022/")

p <- read_csv(paste0("Pitch Level Data/Pitcher Comps 2022/", files[1]))

for(i in 2:length(files)) {
  p <- rbind(p,
             read_csv(paste0("Pitch Level Data/Pitcher Comps 2022/",files[i])))
}

add <- stats_2022 %>% 
  select(player_id, woba)

p <- p %>% 
  left_join(add, by = c("pitcher...8" = "player_id")) %>% 
  mutate(ID = case_when(woba > 0.330 ~ "Bad",
                        woba < 0.280 ~ "Great",
                        TRUE ~ "Decent"),
         win_exp_added = case_when(inning_topbot == "Bot"  ~ -delta_home_win_exp,
                                   inning_topbot == "Top"  ~ delta_home_win_exp),
         run_exp_added = -delta_run_exp,
         ra_on_play = post_bat_score - bat_score) %>% 
  rename(player_id = pitcher...8,
         hitter = stand,
         pitch_speed = release_speed)

empty <- p %>% 
  filter(is.na(on_3b) & is.na(on_2b) & is.na(on_1b)) %>% 
  select(ID, player_name, player_id, game_date, pitch_type, pitch_name,
         pitch_speed, spin_axis, pfx_x:plate_z, ra_on_play,
         release_pos_x, release_pos_z, bb_type:strikes, pitch_number,
         hitter, home_team:type, outs_when_up, inning, ra_on_play,
         run_exp_added, win_exp_added,
         sz_top, sz_bot, hit_distance_sc:release_extension,
         estimated_ba_using_speedangle:woba_value, at_bat_number) %>% 
  mutate(pitch_name = str_replace(pitch_name, "4-Seam Fastball", "4-Seam"))


# LHP Pitch Data (36 Pitchers) ####

files2 <- list.files("Pitch Level Data/LHP Data/")

pitchers2 <- read_csv(paste0("Pitch Level Data/LHP Data/", files2[1]))

for(i in 2:length(files2)) {
  pitchers2 <- rbind(pitchers2,
                    read_csv(paste0("Pitch Level Data/LHP Data/",files2[i])))
}

add2 <- stats_2022 %>% 
  select(player_id, woba)


pitchers2 <- pitchers2 %>% 
  left_join(add2, by = c("pitcher...8" = "player_id")) %>% 
  mutate(win_exp_added = case_when(inning_topbot == "Bot"  ~ -delta_home_win_exp,
                                   inning_topbot == "Top"  ~ delta_home_win_exp),
         run_exp_added = -delta_run_exp,
         ra_on_play = post_bat_score - bat_score) %>% 
  rename(player_id = pitcher...8,
         hitter = stand,
         pitch_speed = release_speed) %>% 
  select(woba, player_name, player_id, p_throws, game_date, pitch_type, pitch_name,
         pitch_speed, spin_axis, pfx_x:plate_z, ra_on_play,
         release_pos_x, release_pos_z, bb_type:strikes, pitch_number,
         hitter, home_team:type, outs_when_up, inning, ra_on_play,
         run_exp_added, win_exp_added,
         sz_top, sz_bot, hit_distance_sc:release_extension,
         estimated_ba_using_speedangle:woba_value, at_bat_number, 
         description, events, des, on_3b:on_1b, outs_when_up, zone, batter) %>% 
  mutate(pitch_name = str_replace(pitch_name, "4-Seam Fastball", "4-Seam")) %>% 
  filter(!is.na(pitch_type)) %>% 
  mutate(pitch_type = str_replace(pitch_type, "CS", "CU"),
         pitch_name = str_replace(pitch_name, "Slow Curve", "Curveball"),
         pitch_type = str_replace(pitch_type, "KC", "CU"),
         pitch_name = str_replace(pitch_name, "Knuckle Curve", "Curveball"))

pitchers2 <- pitchers2 %>% 
  mutate(speed_change = pitch_speed - mean(pitch_speed), 
         .by = c(pitch_type, game_date, player_id) ) %>% 
  mutate(pfx_total = sqrt(pfx_x^2 + pfx_z^2)) %>% 
  mutate(break_change = pfx_total - mean(pfx_total), 
         .by = c(pitch_type, game_date, player_id) ) %>% 
  mutate(distance = sqrt(plate_x^2 + (plate_z - (sz_top + sz_bot)/2)^2)) %>% 
  mutate(
    on_3b = case_when(
      on_3b > 0 ~ 1,
      TRUE ~ 0),
    on_2b = case_when(
      on_2b > 0 ~ 1,
      TRUE ~ 0),
    on_1b = case_when(
      on_1b > 0 ~ 1,
      TRUE ~ 0)
  ) %>% 
  mutate(base_out = paste0(outs_when_up, "_", on_1b, on_2b, on_3b)) %>% 
  select(-home_team, -away_team)

pitchers2_overview <- pitchers2 %>% 
  filter(pitch_type  %in% c("FF", "SI", "FC")) %>% 
  group_by(player_id, pitch_type) %>% 
  summarize(Pitches = n()) %>% 
  pivot_wider(names_from = pitch_type, values_from = Pitches)
  
pitchers2_overview <- pitchers2_overview %>% 
  replace(is.na(pitchers2_overview), 0) %>% 
  mutate(fb_type = case_when(FF >= SI & FF >= FC ~ "FF",
                          SI > FF & SI >= FC ~ "SI",
                          FC > FF & FC > SI ~ "FC")) %>% 
  select(player_id, fb_type)

pitchers2 <- pitchers2 %>% 
  left_join(pitchers2_overview, by = c("player_id" = "player_id"))
  

pitchers2_fb <- pitchers2 %>%
  filter(pitch_type  == fb_type) %>% 
  summarize(fb_v = mean(pfx_z),
         fb_h = mean(pfx_x),
        .by = c(game_date, player_id))

pitchers2 <- pitchers2 %>% 
  left_join(pitchers2_fb, by = c("game_date" = "game_date",
                                 "player_id" = "player_id")) %>% 
  mutate(tunnel_z = plate_z - pfx_z + fb_v,
         tunnel_x = plate_x - pfx_x + fb_h) %>%
  mutate(whiff = description == "swinging_strike",
         whiff = as.character(whiff)) %>% 
  filter(!is.na(pitch_type),
         pitch_type != "PO")

pitchers2_diffs <- pitchers2 %>% 
  filter(pitch_type == fb_type) %>% 
  group_by(player_id, game_date) %>% 
  summarize(speed = mean(pitch_speed),
            horz = mean(pfx_x),
            ivb = mean(pfx_z))

pitchers2 <- pitchers2 %>% 
  left_join(pitchers2_diffs, by = c("game_date" = "game_date",
                                   "player_id" = "player_id")) %>% 
  mutate(speed_fb_diff = pitch_speed - speed,
         pfx_x_fb_diff = pfx_x - horz,
         pfx_z_fb_diff = pfx_z - ivb) %>% 
  select(-speed, -horz, -ivb) %>% 
  mutate(zone = as.character(zone))
  


# RHP Pitch Data (85 Pitchers) ####

files3 <- list.files("Pitch Level Data/RHP Data/")

pitchers3 <- read_csv(paste0("Pitch Level Data/RHP Data/", files3[1]))

for(i in 2:length(files3)) {
  pitchers3 <- rbind(pitchers3,
                     read_csv(paste0("Pitch Level Data/RHP Data/",files3[i])))
}

add3 <- stats_2022 %>% 
  select(player_id, woba)


pitchers3 <- pitchers3 %>% 
  left_join(add3, by = c("pitcher...8" = "player_id")) %>% 
  mutate(win_exp_added = case_when(inning_topbot == "Bot"  ~ -delta_home_win_exp,
                                   inning_topbot == "Top"  ~ delta_home_win_exp),
         run_exp_added = -delta_run_exp,
         ra_on_play = post_bat_score - bat_score) %>% 
  rename(player_id = pitcher...8,
         hitter = stand,
         pitch_speed = release_speed) %>% 
  select(woba, player_name, player_id, p_throws, game_date, pitch_type, pitch_name,
         pitch_speed, spin_axis, pfx_x:plate_z, ra_on_play,
         release_pos_x, release_pos_z, bb_type:strikes, pitch_number,
         hitter, home_team:type, outs_when_up, inning, ra_on_play,
         run_exp_added, win_exp_added,
         sz_top, sz_bot, hit_distance_sc:release_extension,
         estimated_ba_using_speedangle:woba_value, at_bat_number, 
         description, events, des, on_3b:on_1b, outs_when_up, zone, batter) %>% 
  mutate(pitch_name = str_replace(pitch_name, "4-Seam Fastball", "4-Seam")) 

pitchers3 <- pitchers3 %>% 
  mutate(speed_change = pitch_speed - mean(pitch_speed), 
         .by = c(pitch_type, game_date, player_id) ) %>% 
  mutate(pfx_total = sqrt(pfx_x^2 + pfx_z^2)) %>% 
  mutate(break_change = pfx_total - mean(pfx_total), 
         .by = c(pitch_type, game_date, player_id) ) %>% 
  mutate(distance = sqrt(plate_x^2 + (plate_z - (sz_top + sz_bot)/2)^2)) %>% 
  mutate(
    on_3b = case_when(
      on_3b > 0 ~ 1,
      TRUE ~ 0),
    on_2b = case_when(
      on_2b > 0 ~ 1,
      TRUE ~ 0),
    on_1b = case_when(
      on_1b > 0 ~ 1,
      TRUE ~ 0)
  ) %>% 
  mutate(base_out = paste0(outs_when_up, "_", on_1b, on_2b, on_3b)) %>% 
  select(-home_team, -away_team)


pitchers3_overview <- pitchers3 %>% 
  filter(pitch_type  %in% c("FF", "SI", "FC")) %>% 
  group_by(player_id, pitch_type) %>% 
  summarize(Pitches = n()) %>% 
  pivot_wider(names_from = pitch_type, values_from = Pitches)

pitchers3_overview <- pitchers3_overview %>% 
  replace(is.na(pitchers3_overview), 0) %>% 
  mutate(fb_type = case_when(FF >= SI & FF >= FC ~ "FF",
                             SI > FF & SI >= FC ~ "SI",
                             FC > FF & FC > SI ~ "FC")) %>% 
  select(player_id, fb_type)

pitchers3 <- pitchers3 %>% 
  left_join(pitchers3_overview, by = c("player_id" = "player_id"))


pitchers3_fb <- pitchers3 %>%
  filter(pitch_type  == fb_type) %>% 
  summarize(fb_v = mean(pfx_z),
            fb_h = mean(pfx_x),
            .by = c(game_date, player_id))

pitchers3 <- pitchers3 %>% 
  left_join(pitchers3_fb, by = c("game_date" = "game_date",
                                 "player_id" = "player_id")) %>% 
  mutate(tunnel_z = plate_z - pfx_z + fb_v,
         tunnel_x = plate_x - pfx_x + fb_h) %>%
  mutate(whiff = description == "swinging_strike",
         whiff = as.character(whiff)) %>% 
  filter(pitch_type != "NA",
         pitch_type != "PO")

pitchers3_diffs <- pitchers3 %>% 
  filter(pitch_type == fb_type) %>% 
  group_by(player_id, game_date) %>% 
  summarize(speed = mean(pitch_speed),
            horz = mean(pfx_x),
            ivb = mean(pfx_z))

pitchers3 <- pitchers3 %>% 
  left_join(pitchers3_diffs, by = c("game_date" = "game_date",
                                    "player_id" = "player_id")) %>% 
  mutate(speed_fb_diff = pitch_speed - speed,
         pfx_x_fb_diff = pfx_x - horz,
         pfx_z_fb_diff = pfx_z - ivb) %>% 
  select(-speed, -horz, -ivb) %>% 
  mutate(zone = as.character(zone))



# All Pitch Data (121 Pitchers) ####
pitchers4 <- rbind(pitchers2, pitchers3) %>% 
  as.data.frame() %>% 
  arrange(player_id) %>% 
  mutate(ab_id = paste0(game_date, "_", player_id, "_", at_bat_number),
         prev_ab_id = lead(ab_id, 1)) %>% 
  mutate(prev_pitch = ifelse(ab_id == prev_ab_id, lead(pitch_type, 1), NA)) %>% 
  mutate(is_barrel = is.barrel(LA = launch_angle, EV = launch_speed)) %>% 
  mutate(is_strike = ifelse(description  %in% c("called_strike", "swinging_strike",
                                                "foul_tip", "bunt_foul_tip", "foul_bunt",
                                                "swinging_strike_blocked", "missed_bunt"),
                            1, 0)) %>% 
  mutate(is_benefit = ifelse(run_exp_added > 0, 1, 0)) %>% 
  mutate(zone = as.character(zone))


# Batter Data
# URL: https://baseballsavant.mlb.com/leaderboard/custom?year=2022&type=batter&filter=&sort=4&sortDir=desc&min=10&selections=pa,hit,home_run,k_percent,bb_percent,batting_avg,slg_percent,on_base_percent,on_base_plus_slg,r_total_stolen_base,woba,xwoba,exit_velocity_avg,sweet_spot_percent,barrel_batted_rate,solidcontact_percent,flareburner_percent,poorlyunder_percent,poorlytopped_percent,poorlyweak_percent,hard_hit_percent,&chart=false&x=pa&y=pa&r=no&chartType=beeswarm
# Downloaded: December 29, 2023

# batters <- read_csv("~/Downloads/Research/Research/CSVs/batter_stats.csv") %>% 
#   rename(batter_name = `last_name, first_name`,
#          batter_id = player_id, 
#          PA = pa, H = hit, HR = home_run,
#          AVG = batting_avg,
#          SLG = slg_percent,
#          OBP = on_base_percent,
#          OPS = on_base_plus_slg,
#          xwOBA = xwoba,
#          wOBA = woba,
#          SB = r_total_stolen_base,
#          EV = exit_velocity_avg,
#          barrel_rate = barrel_batted_rate) %>% 
#   select(-year, -`...25`)



# App Comp Data ####

full_data <- read_csv("CSVs/all_pitches.csv")

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


comps <- full_data %>% 
  mutate(is_benefit = ifelse(run_exp_added > 0, 1, 0),
         is_barrel = is.barrel(launch_angle, launch_speed),
         player_name = sapply(player_name, swap_names)) %>% 
  group_by(player_name, pitch_type, p_throws, hitter) %>% 
  summarize(Pitches = n(),
            Speed = mean(pitch_speed, na.rm = TRUE),
            "Spin Rate" = mean(release_spin_rate, na.rm = TRUE),
            "X Movement" = mean(pfx_x, na.rm = TRUE),
            "Z Movement" = mean(pfx_z, na.rm = TRUE),
            "Whiff Prop" = mean(whiff, na.rm = TRUE),
            "Benefit Prop" = mean(is_benefit, na.rm = TRUE), 
            "Barrel Prop" = mean(is_barrel, na.rm = TRUE)) %>% 
  rename(Name = player_name) %>% 
  filter(Pitches >= 100)

# 2024 Comps
Data2024 <- read_csv("Pitch Level Data/Data2024.csv") %>% 
  filter(game_year == 2024)
comps <- Data2024 %>% 
  mutate(is_benefit = ifelse(delta_pitcher_run_exp > 0, 1, 0),
         is_barrel = is.barrel(launch_angle, launch_speed),
         whiff = ifelse(description == "swinging_strike", 1, 0),
         player_name = sapply(player_name, swap_names)) %>% 
  group_by(player_name, pitch_type, p_throws, hitter) %>% 
  summarize(Pitches = n(),
            Speed = mean(release_speed, na.rm = TRUE),
            "Spin Rate" = mean(release_spin_rate, na.rm = TRUE),
            "X Movement" = mean(pfx_x, na.rm = TRUE),
            "Z Movement" = mean(pfx_z, na.rm = TRUE),
            "Whiff Prop" = mean(whiff, na.rm = TRUE),
            "Benefit Prop" = mean(is_benefit, na.rm = TRUE), 
            "Barrel Prop" = mean(is_barrel, na.rm = TRUE)) %>% 
  rename(Name = player_name) %>% 
  filter(Pitches >= 100)


# Batter Prediction Averages ####

rhp <- full_data %>% 
  filter(p_throws == "R")

# Batter Whiff
batter_stats <- rhp %>% 
  summarize(rate = mean(whiff),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_whiff = weighted.mean(rate, pitches, na.rm = TRUE),
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
  mutate(pitch_barrel = weighted.mean(rate, pitches, na.rm = TRUE),
         .by = pitch_type) %>% 
  mutate(pred_bbarrel = (pitches / 300)*rate + ((300-pitches)/300)*pitch_barrel) %>% 
  mutate(pred_bbarrel = ifelse(pitches >= 300, rate, pred_bbarrel))

rhp <- rhp %>% 
  left_join(select(batter_stats2, batter, pitch_type, pred_bbarrel), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))


# Benefit
batter_stats3 <- rhp %>% 
  summarize(rate = mean(is_benefit),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_benefit = weighted.mean(rate, pitches, na.rm = TRUE),
         .by = pitch_type) %>% 
  mutate(pred_bbenefit = (pitches / 300)*rate + ((300-pitches)/300)*pitch_benefit) %>% 
  mutate(pred_benefit = ifelse(pitches >= 300, rate, pred_bbenefit))

rhp <- rhp %>% 
  left_join(select(batter_stats3, batter, pitch_type, pred_bbenefit), 
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



lhp <- full_data %>% 
  filter(p_throws == "L")

# Batter Whiff
batter_stats_l <- lhp %>% 
  summarize(rate = mean(whiff),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_whiff = weighted.mean(rate, pitches, na.rm = TRUE),
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
  mutate(pitch_barrel = weighted.mean(rate, pitches, na.rm = TRUE),
         .by = pitch_type) %>% 
  mutate(pred_bbarrel = (pitches / 300)*rate + ((300-pitches)/300)*pitch_barrel) %>% 
  mutate(pred_bbarrel = ifelse(pitches >= 300, rate, pred_bbarrel))

lhp <- lhp %>% 
  left_join(select(batter_stats2_l, batter, pitch_type, pred_bbarrel), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))


# Benefit
batter_stats3_l <- lhp %>% 
  summarize(rate = mean(is_benefit),
            pitches = n(),
            .by = c(batter, pitch_type)) %>% 
  mutate(pitch_benefit = weighted.mean(rate, pitches, na.rm = TRUE),
         .by = pitch_type) %>% 
  mutate(pred_bbenefit = (pitches / 300)*rate + ((300-pitches)/300)*pitch_benefit) %>% 
  mutate(pred_bbenefit = ifelse(pitches >= 300, rate, pred_bbenefit))

lhp <- lhp %>% 
  left_join(select(batter_stats3_l, batter, pitch_type, pred_bbenefit), 
            by = c("batter" = "batter", "pitch_type" = "pitch_type"))


batter_preds_r <- batter_stats %>% 
  left_join(batter_stats2, by = c("batter", "pitch_type")) %>% 
  left_join(batter_stats3, by = c("batter", "pitch_type")) %>% 
  select(batter, pitch_type, pitches, pred_bwhiff, pred_bbarrel, pred_bbenefit) %>% 
  mutate(p_throws = "R")

batter_preds_l <- batter_stats_l %>% 
  left_join(batter_stats2_l, by = c("batter", "pitch_type")) %>% 
  left_join(batter_stats3_l, by = c("batter", "pitch_type")) %>% 
  select(batter, pitch_type, pitches, pred_bwhiff, pred_bbarrel, pred_bbenefit) %>% 
  mutate(p_throws = "L")

batter_hands <- full_data %>% 
  select(batter, p_throws, hitter) %>%
  mutate(bh = paste0(batter, hitter)) %>% 
  distinct(bh, .keep_all = TRUE) %>% 
  select(-bh)

pred_means <- rbind(batter_preds_r, batter_preds_l) %>% 
  left_join(batter_hands, by = c("p_throws", "batter")) %>% 
  filter(!is.na(hitter)) %>% 
  group_by(p_throws, hitter, pitch_type) %>% 
  summarize(whiff_mean = weighted.mean(pred_bwhiff, pitches, na.rm = TRUE),
            barrel_mean = weighted.mean(pred_bbarrel, pitches, na.rm = TRUE),
            benefit_mean = weighted.mean(pred_bbenefit, pitches, na.rm = TRUE))



# Combine Models ####
models_rhp <- read_csv("models_rhp.csv") %>% 
  mutate(PHand = "R")
models_lhp <- read_csv("models_lhp.csv") %>% 
  mutate(PHand  = "L")

models_final <- rbind(models_rhp, models_lhp)



# Zone Data ####

# RHP

models <- read_csv("models_final.csv") %>% 
  filter(PHand == "R") %>% 
  mutate(Pitch = str_replace(Pitch, "FF", "fastball"),
         Pitch = str_replace(Pitch, "SL", "slider"),
         Pitch = str_replace(Pitch, "CU", "curveball"),
         Pitch = str_replace(Pitch, "CH", "changeup"),
         Pitch = str_replace(Pitch, "FC", "cutter"),
         Pitch = str_replace(Pitch, "SI", "sinker"),
         Pitch = str_replace(Pitch, "FS", "splitter")) %>% 
  mutate(Hand = ifelse(BHand == "R", "right", "left")) %>% 
  mutate(term = case_when(term == "pred" & Response == "whiff" ~ "pred_bwhiff",
                          term == "pred" & Response == "barrel" ~ "pred_bbarrel",
                          term == "pred" & Response == "benefit" ~ "pred_bbenefit",
                          TRUE ~ term))

zones <- models %>%
  filter(term %in% c("zone1", "zone2", "zone3", "zone4", 
                     "zone6", "zone7", "zone8", "zone9"))


zone_data <- data.frame(x = seq(-2, 2, .1),
                        z = seq(-2.5, 1.5, .1),
                        response = rep(c("whiff", "benefit", "barrel"), length.out = 41),
                        hand = rep(c("right", "left"), length.out = 41),
                        pitch = rep(c("fastball", "slider", "curveball", "changeup", "cutter", "sinker", "splitter"), length.out = 41)) %>%
  tidyr::expand(x, z, response, hand, pitch) %>%
  mutate(pred = NA)

for(response in c("whiff", "benefit", "barrel")) {
  for(hand in c("right", "left")) {
    for(pitch in c("fastball", "slider", "curveball", "changeup", "cutter", "sinker", "splitter")) {
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


# LHP

models2 <- read_csv("models_final.csv") %>% 
  filter(PHand == "L") %>% 
  mutate(Pitch = str_replace(Pitch, "FF", "fastball"),
         Pitch = str_replace(Pitch, "SL", "slider"),
         Pitch = str_replace(Pitch, "CU", "curveball"),
         Pitch = str_replace(Pitch, "CH", "changeup"),
         Pitch = str_replace(Pitch, "FC", "cutter"),
         Pitch = str_replace(Pitch, "SI", "sinker")) %>% 
  mutate(Hand = ifelse(BHand == "R", "right", "left")) %>% 
  mutate(term = case_when(term == "pred" & Response == "whiff" ~ "pred_bwhiff",
                          term == "pred" & Response == "barrel" ~ "pred_bbarrel",
                          term == "pred" & Response == "benefit" ~ "pred_bbenefit",
                          TRUE ~ term))

zones2 <- models2 %>%
  filter(term %in% c("zone1", "zone2", "zone3", "zone4", 
                     "zone6", "zone7", "zone8", "zone9"))


zone_data2 <- data.frame(x = seq(-2, 2, .1),
                        z = seq(-2.5, 1.5, .1),
                        response = rep(c("whiff", "benefit", "barrel"), length.out = 41),
                        hand = rep(c("right", "left"), length.out = 41),
                        pitch = rep(c("fastball", "slider", "curveball", "changeup", "cutter", "sinker"), length.out = 41)) %>%
  tidyr::expand(x, z, response, hand, pitch) %>%
  mutate(pred = NA)

for(response in c("whiff", "benefit", "barrel")) {
  for(hand in c("right", "left")) {
    for(pitch in c("fastball", "slider", "curveball", "changeup", "cutter", "sinker", "splitter")) {
      test_data <- filter(models2, Hand == hand, Pitch == pitch, Response == response)
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
      
      zone_data2[zone_data2$response == response & zone_data2$hand == hand & zone_data2$pitch == pitch,] <- zone_data2[zone_data2$response == response & zone_data2$hand == hand & zone_data2$pitch == pitch,] %>%
        mutate(pred = x * test_x + z * test_z +
                 x^2 * test_x2 + z^2 * test_z2 +
                 sqrt(x^2 + z^2) * test_prop)
    }
  }
}

# Combining Zone Info

zone_data <- zone_data %>% 
  mutate(phand = "right") %>% 
  rename(bhand = hand)

zone_data2 <- zone_data2 %>% 
  mutate(phand = "left") %>% 
  rename(bhand = hand)

zone_data_full <- rbind(zone_data, zone_data2)

zone_data_full <- zone_data_full %>% 
  select(x, z, phand, response:pred)



# Input Restrictions for App ####

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

all_averages <- rbind(data2021, data2022, data2023) %>% 
  na.omit()

averages_table <- full_data %>% 
  filter(!is.na(pitch_speed)) %>% 
  mutate(pitch_speed = round(pitch_speed, 0)) %>% 
  group_by(p_throws, pitch_type, pitch_speed) %>% 
  summarize(min_x = min(pfx_x, na.rm = TRUE),
            max_x = max(pfx_x, na.rm = TRUE),
            min_z = min(pfx_z, na.rm = TRUE),
            max_z = max(pfx_z, na.rm = TRUE),
            min_spin = min(release_spin_rate, na.rm = TRUE),
            max_spin = max(release_spin_rate, na.rm = TRUE))

# Zone Data Pitch Count ####

zone_data <- read_csv("zone_data.csv")

zone_count <- full_data %>% 
  filter(pitch_type %in% c("FF", "SI", "FC", "SL", "CU", "CH", "FS"),
         !is.na(zone)) %>% 
  group_by(p_throws, hitter, pitch_name, zone) %>% 
  mutate(pitch_name = tolower(pitch_name),
         p_throws = ifelse(p_throws == "L", "left", "right"),
         hitter = ifelse(hitter == "L", "left", "right"),
         pitch_name = case_when(pitch_name == "4-seam" ~ "fastball",
                                pitch_name == "split-finger" ~ "splitter",
                                TRUE ~ pitch_name)) %>% 
  summarize(N = n()) %>% 
  ungroup()



zone_data2 <- zone_data %>% 
  select(-...1) %>% 
  mutate(zone = case_when(
    x >= -1 & x < -0.33 & z <= 1 & z > 0.33  ~ 3, 
    x >= -1 & x < -0.33 & z <= 0.33 & z >= -0.33 ~ 6,  
    x >= -1 & x < -0.33 & z < -0.33 & z >= -1 ~ 9,
    
    x >= -0.33 & x <= 0.33 & z <= 1 & z > 0.33  ~ 2, 
    x >= -0.33 & x <= 0.33 & z <= 0.33 & z >= -0.33 ~ 5,  
    x >= -0.33 & x <= 0.33 & z < -0.33 & z >= -1 ~ 8,
    
    x > 0.33 & x <= 1 & z <= 1 & z > 0.33  ~ 1, 
    x > 0.33 & x <= 1 & z <= 0.33 & z >= -0.33 ~ 4,  
    x > 0.33 & x <= 1 & z < -0.33 & z >= -1 ~ 7,
    
    x > 0 & z >= 0 ~ 11,
    x <= 0 & z > 0 ~ 12,
    x >= 0 & z < 0 ~ 13,
    x < 0 & z <= 0 ~ 14)) %>% 
  left_join(zone_count, by = c("phand" = "p_throws", "bhand" = "hitter", 
                               "pitch" = "pitch_name", "zone" = "zone"))



# zone_data2 %>% 
#   mutate(zone = as.factor(zone)) %>% 
#   ggplot() +
#   geom_point(aes(x = x, y = z, color = zone))




# CSV Exports ####

# Pitch Arsenal CSV
write.csv(arsenal, "arsenal.csv")

# Pitcher Seasonal Stats CSV
write.csv(stats_2022, "season_stats.csv")

# Pitch-by-Pitch "Pitchers" CSV
write.csv(pitchers, "pitcher_comps.csv")

# Bases Empty CSV
write.csv(empty, "bases_empty.csv")

# Pitch-by-Pitch Lefties CSV
write.csv(pitchers2, "lhp_pitches.csv")

# Pitch-by-Pitch Righties CSV
write.csv(pitchers3, "rhp_pitches.csv")

# Pitch-by-Pitch All CSV
write.csv(pitchers4, "all_pitches.csv", row.names = FALSE)

# Batter Stats
# write.csv(batters, "batter_stats.csv")

# Shiny App Comps
write.csv(comps, "Pitcher_App_Comps.csv",
          row.names = FALSE)

# Batter Prediction Averages
write.csv(pred_means, "pred means.csv")

# Zone Data
write.csv(zone_data_full, "zone_data.csv")

# Zone Data (With N)
write.csv(zone_data2, "zone_data_pc.csv", row.names = FALSE)

# Movement Data (Input Ranges)
write.csv(averages_table, "Movement.csv", row.names = FALSE)

# Final Model
write.csv(models_final, "models_final.csv", row.names = FALSE)
