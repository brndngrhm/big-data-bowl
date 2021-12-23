
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
library(ggimage)
library(RCurl)

source(here("R", "util.R"))

#------------------------------------------------
# load data ----

# read in analysis data files
file_names <- 
  c("kickoff_all", "kickoffs", "pbp_kickoff", "scouting_kickoff", "tracking_kickoff", "tracking_kickoff_ball")

list.files(path = here("data", "analysis"), pattern = ".feather") %>%
  map(., ~load_data(type = "analysis", file_name = .x)) %>% 
  set_names(nm = file_names) %>% 
  list2env(., envir = .GlobalEnv)

remove(tracking_kickoff_ball, scouting_kickoff, kickoff_all, tracking_kickoff)

#------------------------------------------------
# combine data ----

model_data_prep <- 
  kickoffs %>%
  filter(return_type %in% c("Endzone Return", "Touchback")) %>%
  left_join(., pbp_kickoff, c("game_id" = "old_game_id", "play_id" = "play_id")) %>% 
  mutate(recieving_team_timeouts_remaining = ifelse(recieving_team_name == home_team_abbr, home_timeouts_remaining, away_timeouts_remaining)) %>%
  select(-c(play_description, down, yards_to_go, game_clock, possession_team, special_teams_play_type, special_teams_result, kicker_id, returner_id, kick_blocker_id, 
            yardline_side, yardline_number, starts_with("penalty"), starts_with("pre_snap"), pass_result, kick_length, kick_return_yardage, play_result, 
            absolute_yardline_number, return_start, starting_yardline, diff_from_default, game_time_eastern, home_team_abbr, visitor_team_abbr, game_datetime,
            teams, recieving_team, recieving_team_name, kicking_team_name, epa, drive_start_yard_line, home_timeouts_remaining, away_timeouts_remaining)) %>% 
  select(game_id, play_id, return_type, everything())

factor_cols <- 
  c("quarter", "season", "game_hour", "game_tod", "primetime_ind", 
    "overseas_game_ind", "holiday", "home_team_recieving_ind", "recieving_team_timeouts_remaining",
    "season_type", "game_half", "stadium", "surface", "roof", "div_game")

model_data <- 
  model_data_prep %>%
  mutate(return_type = factor(return_type, levels = c("Endzone Return", "Touchback")),
         game_date = as.Date(game_date)) %>%
  mutate_at(factor_cols, factor)

write_feather(model_data, path = here("data", "model", "kickoffs_model.feather"))
