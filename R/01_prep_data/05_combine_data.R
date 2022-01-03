
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

source(here::here("R", "util.R"))

#------------------------------------------------
# load data ----

# read in analysis data files
file_names <- 
  c("kickoff_all", "kickoffs", "kickoff_tracking_avg_speed", "pbp_kickoff", "scouting_kickoff", "tracking_kickoff", "tracking_kickoff_ball")

list.files(path = here::here("data", "analysis"), pattern = ".feather") %>%
  map(., ~load_data(type = "analysis", file_name = .x)) %>% 
  set_names(nm = file_names) %>% 
  list2env(., envir = .GlobalEnv)

remove(kickoff_all, tracking_kickoff, tracking_kickoff_ball)

#------------------------------------------------
# combine data and remove some columns ---- 
model_data_prep <- 
  kickoffs %>%
  filter(return_type %in% c("Endzone Return", "Touchback")) %>%
  left_join(., pbp_kickoff, c("game_id" = "old_game_id", "play_id" = "play_id")) %>% 
  left_join(., scouting_kickoff %>% select(play_id, game_id, kick_direction_actual), c("game_id", "play_id")) %>% 
  left_join(., kickoff_tracking_avg_speed, by = c("game_id" = "game_id", "play_id" = "play_id", "kicking_team_name" = "player_team")) %>% 
  mutate(recieving_team_timeouts_remaining = ifelse(recieving_team_name == home_team_abbr, home_timeouts_remaining, away_timeouts_remaining)) %>%
  select(-c(play_description, down, yards_to_go, game_clock, possession_team, special_teams_play_type, special_teams_result, kicker_id, returner_id, kick_blocker_id, 
            yardline_side, starts_with("penalty"), starts_with("pre_snap"), pass_result, kick_return_yardage, play_result, 
            absolute_yardline_number, return_start, starting_yardline, diff_from_default, game_time_eastern, home_team_abbr, visitor_team_abbr, game_datetime,
            teams, recieving_team, recieving_team_name, kicking_team_name, epa, drive_start_yard_line, home_timeouts_remaining, away_timeouts_remaining,
            stadium, quarter_seconds_remaining, recieving_team_score_diff,
            season_type, home_team_recieving_ind, holiday, 
            overseas_game_ind, month, day_nm, game_tod, game_hour, week, season,
            prev_cuml_endzone_return_yards, prev_cuml_return_yards_allowed)) %>% 
  select(game_id, play_id, return_type, everything()) %>%
  rename(kickoff_from_yardline = yardline_number,
         prev_cuml_avg_ball_in_air_speed_kicking_team = prev_cuml_avg_ball_in_air_speed)

#------------------------------------------------
# set certain columns as factors ----
factor_cols <- 
  c("quarter", "recieving_team_timeouts_remaining", "game_half", "surface", "roof", "div_game", "prev_play_result", "prev_play_lead_change", 
    "prev_endzone_return_attempts", "kick_direction_actual", "kickoff_from_yardline", "primetime_ind")

model_data <- 
  model_data_prep %>%
  mutate(return_type = factor(return_type, levels = c("Endzone Return", "Touchback")),
         game_date = as.Date(game_date),
         prev_play_lead_change = ifelse(is.na(prev_play_lead_change), 0, prev_play_lead_change),
         prev_cuml_avg_ball_in_air_speed_kicking_team = 
           ifelse(is.na(prev_cuml_avg_ball_in_air_speed_kicking_team), 0, prev_cuml_avg_ball_in_air_speed_kicking_team)
         ) %>%
  mutate_at(factor_cols, factor)

str(model_data)

#------------------------------------------------
# check variables relation to outcome and simplify ----
numeric_cols <- 
  model_data %>% 
  select(-c(game_id)) %>%
  select(where(is.numeric)) %>%
  names

categorical_cols <- 
  model_data %>% 
  select(-return_type) %>%
  select(where(is.factor) | where(is.character)) %>%
  names

numeric_boxplots <-
  function(col_name){
    
    # col_name <- "recieving_team_score_diff"
    
    model_data %>%
      select(return_type, one_of({{col_name}})) %>%
      rename(predictor = 2) %>%
      ggplot(., aes(x = return_type, y = predictor, fill = return_type)) + 
      geom_jitter(alpha = .3, color = 'gray') + 
      geom_boxplot(alpha = .75, outlier.shape = NA) + 
      coord_flip() + 
      theme_minimal() + 
      scale_fill_manual(values = c("cornflowerblue", "tomato")) + 
      labs(x = "", y = "", title = paste("Return type vs", {{col_name}}))
  }

categorical_bar_plots <-
  function(col_name){
    
    model_data %>%
      select(return_type, one_of({{col_name}})) %>%
      rename(predictor = 2) %>%
      group_by(predictor, return_type) %>%
      tally() %>%
      mutate(freq = n / sum(n)) %>%
      ggplot(., aes(x = reorder(predictor, -freq), y = freq, fill = return_type, label = paste0(paste0(round(freq*100, 2), "%"), "\n(n = ", n, ")"))) + 
      geom_col(position = 'dodge', alpha = .75) + 
      geom_text(position = position_dodge(width = .9), vjust = -0.5) + 
      theme_minimal() + 
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .10)) + 
      scale_fill_manual(values = c("cornflowerblue", "tomato")) + 
      labs(x = "", y = "Count", title = "80% of Endzone Kickoffs Result in a Touchback") + 
      labs(x = "", y = "", title = paste("Return type vs", {{col_name}}))
  }

map(numeric_cols, ~numeric_boxplots(.x))

map(categorical_cols, ~categorical_bar_plots(.x))

#------------------------------------------------
# save datasets ----
write_feather(model_data, path = here::here("data", "model", "kickoffs_model.feather"))
