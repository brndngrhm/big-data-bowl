
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
    diff_from_default = starting_yardline - 25) %>%
  left_join(., games %>% 
              mutate(teams = paste(home_team_abbr, visitor_team_abbr, sep = " "),
                     game_tod = case_when(
                       hour(game_time_eastern) == 9 ~ 'morning',
                       hour(game_time_eastern) %in% c(12, 13, 15) ~ 'afternoon',
                       hour(game_time_eastern) %in% c(16, 17) ~ 'late_afternoon',
                       hour(game_time_eastern) %in% c(19, 20, 21, 22) ~ 'night',
                       TRUE ~ "FROG"),
                     ), 
            by = c("game_id")) %>%
  rowwise() %>%
  mutate(recieving_team = ifelse(trimws(str_remove(teams, possession_team)) == trimws(visitor_team_abbr), "visiting_team", "home_team"),
         recieving_team_score_diff = ifelse(recieving_team == visitor_team_abbr,
                                            pre_snap_visitor_score - pre_snap_home_score, 
                                            pre_snap_home_score - pre_snap_visitor_score)) 

# kickoffs without fumbles, OOB,... only returns or touchbacks
kickoffs <- 
  kickoffs_all %>% 
  filter(special_teams_result %in% c("Touchback", "Return"))

# for each kickoff, get the number of attempted endzone returns and cumulative yards
kickoffs_with_returns <-
  kickoffs %>%
  filter(return_type %in% "Endzone Return") %>%
  distinct() %>%
  mutate(recieving_team_name = ifelse(recieving_team == "visiting_team", visitor_team_abbr, home_team_abbr),
         kicking_team_name = ifelse(trimws(str_remove(teams, recieving_team_name)) == trimws(visitor_team_abbr), visitor_team_abbr, home_team_abbr)) %>%
  select(game_id, play_id, recieving_team_name, kick_return_yardage) %>%
  group_by(game_id, recieving_team_name) %>%
  arrange(game_id, recieving_team_name, play_id) %>%
  mutate(endzone_return_attempt = dplyr::row_number(),
         cuml_endzone_return_yards = cumsum(kick_return_yardage),
         prev_cuml_endzone_return_yards = lag(cuml_endzone_return_yards, 1),
         prev_endzone_return_attempts = lag(endzone_return_attempt, 1)) 

# add cumulative kickoff returns
kickoffs <-
  kickoffs %>%
  mutate(recieving_team_name = ifelse(recieving_team == "visiting_team", visitor_team_abbr, home_team_abbr),
         kicking_team_name = ifelse(trimws(str_remove(teams, recieving_team_name)) == trimws(visitor_team_abbr), visitor_team_abbr, home_team_abbr)) %>%
  left_join(., kickoffs_with_returns %>% 
              select(game_id, play_id, prev_endzone_return_attempts, prev_cuml_endzone_return_yards, recieving_team_name),
            by = c("game_id", "play_id", "recieving_team_name"))  %>% 
  group_by(game_id, recieving_team_name) %>%
  tidyr::fill(prev_endzone_return_attempts, prev_cuml_endzone_return_yards, .direction = "down") %>% 
  mutate(prev_endzone_return_attempts = ifelse(is.na(prev_endzone_return_attempts), 0, prev_endzone_return_attempts),
         prev_cuml_endzone_return_yards = ifelse(is.na(prev_cuml_endzone_return_yards), 0, prev_cuml_endzone_return_yards))

# calc avg yards allowed per return at time of kick - not quite right yet
return_yards_allowed <-
  kickoffs %>%
  filter(return_type %in% c("Endzone Return", "Field Return")) %>%
  distinct() %>%
  select(game_id, play_id, teams, recieving_team, recieving_team_name, kicking_team_name, kick_return_yardage, visitor_team_abbr, home_team_abbr) %>%
  group_by(game_id, kicking_team_name) %>% 
  arrange(game_id, play_id, kicking_team_name) %>%
  mutate(cuml_return_yards_allowed = cumsum(kick_return_yardage),
         prev_cuml_return_yards_allowed = lag(cuml_return_yards_allowed, 1),
         return_number = ifelse(!is.na(prev_cuml_return_yards_allowed), row_number()-1, NA_real_),
         rolling_mean_return_yards_allowed = prev_cuml_return_yards_allowed /return_number)

kickoffs <-
  kickoffs %>%
  left_join(., return_yards_allowed %>% 
              select(game_id, play_id, prev_cuml_return_yards_allowed, kicking_team_name, rolling_mean_return_yards_allowed),
            by = c("game_id", "play_id", "kicking_team_name")
  ) %>% 
  group_by(game_id, kicking_team_name) %>%
  tidyr::fill(prev_cuml_return_yards_allowed, .direction = "down") %>% 
  mutate(prev_cuml_return_yards_allowed = ifelse(is.na(prev_cuml_return_yards_allowed), 0, prev_cuml_return_yards_allowed),
         rolling_mean_return_yards_allowed = ifelse(is.na(rolling_mean_return_yards_allowed), 0, rolling_mean_return_yards_allowed),
         home_team_recieving_ind = ifelse(recieving_team_name == home_team_abbr, 1, 0))

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

#------------------------------------------------
# scouting data ----

scouting_kickoff <- 
  PFFScoutingData %>%
  mutate(tracking_kickoff_id = paste0(game_id, play_id)) %>%
  filter(tracking_kickoff_id %in% kickoff_ids)

remove(PFFScoutingData)

#------------------------------------------------
# play by play data from nflfastR ----

# filter play by play by play data for only kickoffs
pbp_kickoff <- 
  pbp %>%
  mutate(old_game_id = as.numeric(old_game_id),
         tracking_kickoff_id = paste0(old_game_id, play_id)) %>%
  filter(tracking_kickoff_id %in% kickoff_ids) %>%
  mutate(surface = case_when(
    surface %in% c("fieldturf", "sportturf", "a_turf", "matrixturf", "astroturf", "fieldturf ") ~ 'turf',
    TRUE ~ surface)
    )

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

# apply indicator to all drives 2018091602, 2018091001, 2018092310, 2019110310, 2018090600, 2018090905 2229
all_drive_results <-
  pbp %>%
  filter(!play_type_nfl %in% c("GAME_START", "END_QUARTER")
         ) %>%
  select(old_game_id, down, play_type, play_id, drive, game_half, qtr, time, play_type_nfl, drive_play_id_started, 
         drive_play_id_ended, fixed_drive_result, drive_end_transition, total_home_score, total_away_score) %>%
  mutate(fixed_drive_result = ifelse(fixed_drive_result %in% c("Touchdown", "Opp touchdown"), "Touchdown", fixed_drive_result)) %>%
  group_by(old_game_id) %>%
  mutate(
    fixed_drive_result_adj = case_when(
      time == "15:00" & qtr %in% c(1,3) & play_type_nfl == "KICK_OFF" ~ "start of half",
      time == "10:00" & play_type_nfl == "KICK_OFF" & game_half == "Overtime" ~ "start of overtime",
      drive == 1 ~ NA_character_,
      TRUE ~ fixed_drive_result)) %>% 
  ungroup() %>%
  group_by(old_game_id, drive) %>%
  mutate(start_of_period_ind = ifelse(fixed_drive_result_adj %in% c("start of half", "start of overtime"), 1, 0),
         first_drive_of_period = max(start_of_period_ind),
         fixed_drive_result_adj_adj = case_when(
           first_drive_of_period == 1  ~  "start of period",
           TRUE ~ fixed_drive_result_adj)) %>% 
  ungroup() %>%
  group_by(old_game_id) %>%
  mutate(
    prev_play_id = lag(play_id, 1),
    prev_play_result = case_when(
      is.na(prev_play_id) ~ NA_character_,
      fixed_drive_result_adj_adj == "start of period" ~ "start of period",
      is.na(prev_play_id) & fixed_drive_result_adj_adj == "start of period" ~ "start of period",
      TRUE ~ lag(fixed_drive_result_adj_adj, 1)),
    prev_play_home_score = lag(total_home_score, 1),
    prev_play_away_score = lag(total_away_score, 1),
    old_game_id = as.numeric(old_game_id)) %>%
  distinct() %>% 
  # select(old_game_id, play_id, play_type, fixed_drive_result, fixed_drive_result_adj, fixed_drive_result_adj_adj, prev_play_result, everything()) %>% View
  select(old_game_id, play_id, prev_play_id, prev_play_result, prev_play_home_score, prev_play_away_score) %>%
  left_join(., lead_changes, by = c("old_game_id" = "old_game_id", "prev_play_id" = "play_id")) %>%
  select(-c(prev_play_id)) %>%
    #fix some specific cases where prior play was 'no play' which messes with the logic above
    mutate(prev_play_result = case_when(
      old_game_id == 2018091605 & play_id == 2362 ~ "Touchdown",
      old_game_id == 2018091605 & play_id == 2589 ~ "Touchdown",
      old_game_id == 2019110310 & play_id == 3247 ~ "Touchdown",
      old_game_id == 2018100711 & play_id == 3440 ~ "Touchdown", 
      old_game_id == 2018101411 & play_id == 707 ~ "Touchdown", 
      old_game_id == 2018102102 & play_id == 1539 ~ "Touchdown", 
      old_game_id == 2018102900 & play_id == 1469 ~ "Field goal", 
      old_game_id == 2018120204 & play_id == 4105 ~ "Touchdown",
      old_game_id == 2019092207 & play_id == 3035 ~ "Touchdown",
      old_game_id == 2019101305 & play_id == 3296 ~ "Touchdown",
      old_game_id == 2019102711 & play_id == 1045 ~ "Touchdown",
      old_game_id == 2018100400 & play_id == 1045 ~ "Touchdown",
      old_game_id == 2019092201 & play_id == 1079 ~ "Field goal",
      old_game_id == 2020091309 & play_id == 964 ~ "Touchdown",
      old_game_id == 2018100400 & play_id == 2014 ~ "Touchdown",
      TRUE ~ prev_play_result
    ))

# filter for just kickoffs
pbp_kickoff <- 
  pbp_kickoff %>%
  left_join(., all_drive_results, by = c("old_game_id", "play_id")) %>%
  select(prev_play_result, prev_play_lead_change = drive_resulted_in_lead_change, everything()) %>%
  select(play_id, old_game_id, season_type, quarter_seconds_remaining, half_seconds_remaining, half_seconds_remaining, game_half,
         drive_start_yard_line, away_timeouts_remaining, home_timeouts_remaining, stadium, surface, roof, div_game, prev_play_result, prev_play_lead_change, epa) %>%
  mutate(prev_play_result = ifelse(is.na(prev_play_result), "start of period", prev_play_result))

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
