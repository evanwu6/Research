# Libraries ####
library(tidyverse)
library(readxl)
library(ggforce)
library(knitr)
library(forcats)

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
         estimated_ba_using_speedangle:woba_value, at_bat_number) %>% 
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


# CSV Exports ####

# Pitch Arsenal CSV
write.csv(arsenal, "arsenal.csv")

# Pitcher Seasonal Stats CSV
write.csv(stats_2022, "season_stats.csv")

# Pitch-by-Pitch "Pitchers" CSV
write.csv(pitchers, "pitcher_comps.csv")
