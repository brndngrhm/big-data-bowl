
library(tidyverse)
library(lubridate)
library(stringr)
library(reactable)
library(highcharter)
library(purrr)
library(here)
library(readr)
library(feather)
library(skimr)
library(janitor)
library(timeDate)
library(nflfastR)

source(here("R", "util.R"))

#------------------------------------------------
# load data ----

file_names <- 
  c("games", "players", "plays", "PFFScoutingData")
  
setdiff(
  list.files(path = here("data", "clean"), pattern = ".feather"),
  list.files(path = here("data", "clean"), pattern = "tracking")
  ) %>%
  map(., ~load_data(type = "clean", file_name = .x)) %>% 
  set_names(nm = file_names) %>% 
  list2env(., envir = .GlobalEnv)

tracking_raw <-
  load_data(type = "clean", file_name = "tracking.feather")

# play by play data from nflfastR; read from raw/, no prior cleaning done
pbp <- 
  load_data(type = "raw", file_name = "nflfastR_data.feather")

#------------------------------------------------
# kickoff data ----

# all kickoffs, with some additional variables added around the type and length of return
kickoffs_all <- 
  plays %>% 
  filter(special_teams_play_type == "Kickoff") %>% 
  mutate(
    return_start = 100 - (yardline_number + kick_length),
    return_type = case_when(
      special_teams_result == "Return" & return_start <= 0 ~ "Endzone Return",
      special_teams_result == "Return" & return_start > 0 ~ "Field Return",
      special_teams_result == "Touchback" ~ "Touchback",
      TRUE ~ "FROG"),
    starting_yardline = case_when(
      special_teams_result == "Touchback" ~ 25,
      special_teams_result == "Return" ~ return_start + kick_return_yardage,
      TRUE ~ NA_real_),
    diff_from_default = starting_yardline - 25)%>%
  left_join(., games %>% 
              mutate(teams = paste(home_team_abbr, visitor_team_abbr, sep = " "),
                     game_tod = case_when(
                       hour(game_time_eastern) == 9 ~ 'morning',
                       hour(game_time_eastern) %in% c(12, 13, 15) ~ 'afternoon',
                       hour(game_time_eastern) %in% c(16, 17) ~ 'late_afternoon',
                       hour(game_time_eastern) %in% c(19, 20, 21, 22) ~ 'night',
                       TRUE ~ "FROG")), by = c("game_id")) %>%
  rowwise() %>%
  mutate(recieving_team = ifelse(trimws(str_remove(teams, possession_team)) == trimws(visitor_team_abbr), "visiting_team", "home_team"),
         recieving_team_score_diff = ifelse(recieving_team == visitor_team_abbr,
                                            pre_snap_visitor_score - pre_snap_home_score, 
                                            pre_snap_home_score - pre_snap_visitor_score)) 

# kickoffs without fumbles, OOB,... only returns or touchbacks
kickoffs <- 
  kickoffs_all %>% 
  filter(special_teams_result %in% c("Touchback", "Return"))

# for eac kickoff, get the number of attempted endzone returns and cumulative yards
kickoffs_with_returns <-
  kickoffs %>%
  filter(return_type %in% "Endzone Return") %>%
  distinct() %>%
  select(game_id, play_id, kick_return_yardage) %>%
  group_by(game_id) %>%
  arrange(game_id, play_id) %>%
  mutate(endzone_return_attempt = dplyr::row_number(),
         cuml_endzone_return_yards = cumsum(kick_return_yardage),
         prev_cuml_endzone_return_yards = lag(cuml_endzone_return_yards, 1),
         prev_endzone_return_attempts = lag(endzone_return_attempt, 1)
  )

# add cumulative kickoff returns
kickoffs <-
  kickoffs %>%
  left_join(., kickoffs_with_returns %>% 
              select(game_id, play_id, prev_endzone_return_attempts, prev_cuml_endzone_return_yards),
            by = c("game_id", "play_id")
  )  %>% 
  group_by(game_id) %>%
  tidyr::fill(prev_endzone_return_attempts, prev_cuml_endzone_return_yards, .direction = "down") %>% 
  mutate(prev_endzone_return_attempts = ifelse(is.na(prev_endzone_return_attempts), 0, prev_endzone_return_attempts),
         prev_cuml_endzone_return_yards = ifelse(is.na(prev_cuml_endzone_return_yards), 0, prev_cuml_endzone_return_yards))

# # calc avg yards allowed per return - adjust this to be as of time of kick
# return_yards_allowed <- 
#   kickoffs %>%
#   mutate(recieving_team_name = ifelse(recieving_team == "visiting_team", visitor_team_abbr, home_team_abbr)) %>%
#   group_by(recieving_team_name) %>%
#   summarise(mean_return_yards_allowed = mean(kick_return_yardage, na.rm = T))

# kickoffs <-
#   kickoffs %>%
#   left_join(., return_yards_allowed, by = c("recieving_team_name" == ))

# play_ids for every kickoff play; used to subset tracking data
kickoff_ids <- 
  kickoffs_all %>% 
  ungroup() %>%
  select(game_id, play_id) %>% 
  distinct() %>%
  mutate(kickoff_key = paste0(game_id, play_id)) %>% 
  select(kickoff_key) %>% 
  pull(kickoff_key)

#------------------------------------------------
# tracking data ----

# assign team name to each player in tracking data
player_team <- 
  tracking_raw %>%
  filter(display_name != "football") %>%
  select(nfl_id, game_id, display_name, jersey_number, team) %>%
  distinct() %>%
  left_join(., games %>% select(game_id, season, ends_with("abbr")), by = c("game_id")) %>%
  mutate(player_team = ifelse(team == "home", home_team_abbr, visitor_team_abbr)) %>% 
  select(-ends_with("abbr"), -team)

# add player team to tracking data
tracking_all <- 
  tracking_raw %>% 
  filter(display_name != "football") %>%
  left_join(., player_team %>% 
              select(nfl_id, game_id, player_team), 
            by = c("nfl_id", "game_id")) 

# get ball position - used to ID when plays stop
tracking_kickoff_ball <- 
  tracking_raw %>%
  filter(display_name == "football") %>% 
  mutate(tracking_kickoff_id = paste0(game_id, play_id)) %>%
  filter(tracking_kickoff_id %in% kickoff_ids)

remove(player_team)
remove(tracking_raw)

# subset the player tracking data to only include kickoffs
tracking_kickoff <- 
  tracking_all %>% 
  mutate(tracking_kickoff_id = paste0(game_id, play_id)) %>%
  filter(tracking_kickoff_id %in% kickoff_ids)

remove(tracking_all)

# assign each frame to an event; this lets us calculate kickoff hang time, and get avg player speed/acceleration...within each event type
kickoff_events <- 
  tracking_kickoff %>%
  select(game_id, play_id, frame_id, event) %>%
  mutate(event_phase = case_when(
    event == "None" & frame_id != 1 ~ NA_character_,
    event == "None" & frame_id == 1 ~ "pre_kick",
    TRUE ~ event)) %>%
  distinct() %>%
  arrange(game_id, play_id, frame_id) %>%
  group_by(game_id, play_id) %>%
  tidyr::fill(event_phase, .direction = "down")

tracking_kickoff <-
  tracking_kickoff %>%
  left_join(., kickoff_events %>% select(-event), by = c("game_id", "play_id", "frame_id")) 

remove(kickoff_events)

kickoff_sequences <-
  tracking_kickoff %>%
  select(game_id, play_id, event_phase) %>%
  group_by(game_id, play_id) %>%
  distinct() %>% 
  summarise(event = paste(event_phase, collapse = ', ')) %>%
  ungroup() %>%
  group_by(event) %>% 
  tally(sort = T, name = "kickoff_event_sequences") %>%
  mutate(freq = kickoff_event_sequences / sum(kickoff_event_sequences))

# tracking_kickoff %>%
#   select(game_id, play_id, event_phase) %>%
#   group_by(game_id, play_id) %>%
#   distinct() %>%
#   group_by(event_phase) %>% 
#   tally(sort = T, name = "kickoff_events") %>% View
# 
# kickoff_tracking %>%
#   select(game_id, play_id, event) %>%
#   distinct() %>%
#   group_by(event) %>% 
#   tally() %>% View

#------------------------------------------------
# scouting data ----

scouting_kickoff <- 
  scouting %>%
  mutate(tracking_kickoff_id = paste0(game_id, play_id)) %>%
  filter(tracking_kickoff_id %in% kickoff_ids)

remove(scouting)

#------------------------------------------------
# play by play data from nflfastR ----

# filter play by play by play data for only kickoffs
pbp_kickoff <- 
  pbp %>%
  mutate(old_game_id = as.numeric(old_game_id),
         tracking_kickoff_id = paste0(old_game_id, play_id)) %>%
  filter(tracking_kickoff_id %in% kickoff_ids)

# create indicator for when a scoring drive results in a lead change
lead_changes <-
  pbp %>%
  select(old_game_id, play_id, drive, total_home_score, total_away_score, home_team, away_team) %>%
  mutate(leader = case_when(
    total_away_score == total_home_score ~ "tie",
    total_away_score < total_home_score ~ home_team,
    total_away_score > total_home_score ~ away_team,
    TRUE ~ "FROG"),
    lead_change = ifelse(leader != lag(leader), 1, 0),
    old_game_id = as.numeric(old_game_id)) %>%
  group_by(old_game_id, drive) %>%
  mutate(drive_resulted_in_lead_change = max(lead_change))

# apply indicator to all drives
all_drive_results <- 
  pbp %>%
  select(old_game_id, down, play_type, play_id, drive_play_id_started, drive_play_id_ended, fixed_drive_result, total_home_score, total_away_score) %>% 
  mutate(prev_play_id = lag(play_id, 1),
         prev_play_result = lag(fixed_drive_result, 1),
         prev_play_home_score = lag(total_home_score, 1),
         prev_play_away_score = lag(total_away_score, 1),
         old_game_id = as.numeric(old_game_id)) %>%
  distinct() %>% 
  select(old_game_id, play_id, prev_play_id, prev_play_result, prev_play_home_score, prev_play_away_score) %>%
  left_join(., lead_changes, by = c("old_game_id" = "old_game_id", "prev_play_id" = "play_id")) %>%
  select(-prev_play_id)

# filter for just kickoffs
pbp_kickoff <- 
  pbp_kickoff %>%
  left_join(., all_drive_results, by = c("old_game_id", "play_id")) %>%
  select(prev_play_result, prev_play_lead_change = drive_resulted_in_lead_change, everything())

remove(pbp)
remove(all_drive_results)
remove(lead_changes)
#------------------------------------------------
# save data ----

write_feather(x = kickoffs_all, path = here("data", "analysis", "kickoff_all_analysis.feather"))
write_feather(x = kickoffs, path = here("data", "analysis", "kickoff_analysis.feather"))
write_feather(x = tracking_kickoff, path = here("data", "analysis", "tracking_kickoff_analysis.feather"))
write_feather(x = tracking_kickoff_ball, path = here("data", "analysis", "tracking_kickoff_ball_analysis.feather"))
write_feather(x = scouting_kickoff, path = here("data", "analysis", "scouting_analysis.feather"))
write_feather(x = pbp_kickoff, path = here("data", "analysis", "pbp_analysis.feather"))
