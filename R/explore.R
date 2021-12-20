
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


#------------------------------------------------
# custom functions ----
round_numerics <- 
  function(data){
    data %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
  }

add_table <- 
  function(data){
    data %>%
      round_numerics() %>%
      reactable::reactable(., fullWidth = F, resizable = T, filterable = T, highlight = T, defaultPageSize = 10, 
                           showSortIcon = T, searchable = T, striped = T, compact = T, defaultExpanded = T)
  }

load_clean_data <- 
  function(file_name){
    feather::read_feather(here("data", "clean", {{file_name}}))
  }

#------------------------------------------------
# load data ----
file_names <- 
  c("games", "players", "plays", "PFFScoutingData")

# read in .feather files
data <- 
  setdiff(list.files(path = here("data", "clean"), pattern = ".feather"),
          list.files(path = here("data", "clean"), pattern = "tracking")) %>%
  map(., ~load_clean_data(.x)) %>% 
  set_names(nm = file_names)

data$plays %>% 
  filter(special_teams_play_type == "Kickoff") %>%
  group_by(special_teams_result) %>% tally()

tracking_file_names <-
  c("tracking2018", "tracking2019", "tracking2020")

# read in .feather files
tracking_data <-
  setdiff(list.files(path = here("data", "raw"), pattern = "tracking"),
          list.files(path = here("data", "raw"), pattern = ".csv")) %>%
  map(., ~load_data(.x)) %>%
  set_names(nm = tracking_file_names)

#------------------------------------------------
# datasets ----

games <- 
  data$games

games %>% 
  add_table()

players <- 
  data$players

plays <- 
  data$plays

scouting <- 
  data$PFFScoutingData 

scouting %>% 
  add_table()

# tracking_2018 <- 
#   tracking_data$tracking2018 
# 
# tracking_2018 %>% 
#   janitor::clean_names() %>%
#   filter(game_id == "2018090600",
#          play_id == 677
#          # display_name == "Foye Oluokun"
#          ) %>%
#   arrange(display_name, frame_id) %>%
#   # sample_n(100) %>%
#   add_table()

#------------------------------------------------
# focus on kickoffs ----
# why give up free yards?
# for kickoffs, does starting yardline even matter? for that drive? for the outcome of the game?
#   look at expected drive success rate for each starting yardline; create a curve?
# What are the avg yards gained from an endzone kickoff return?
# Given a kickoff return happens from the endzone, whats the likelihood of getting to at least the 25 yard line (i.e. break even)?
# are endzone kickoff returns more likely to happen at certain times or with certain scores?
#   if so, how could a kicking team take advantage of this? if situations are identified where a receiver is more likely
#   to return from the endzone, how can the kicking team minimize the return yaradage? 
# does likelihood of getting to at least the 25 yard line change depending on the strength of the *receiving* team special teams?
# does likelihood of getting to at least the 25 yard line change depending on the strength of the *kicking* team special teams?

data$plays %>% 
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards),
         game_id == "2018090600"
  ) %>%  
  mutate(
    return_start = 100 - (yardline_number + kick_length),
    starting_yardline = case_when(
      special_teams_result == "Touchback" ~ 25,
      special_teams_result == "Return" ~ return_start + kick_return_yardage,
      TRUE ~ NA_real_),
    diff_from_default = starting_yardline - 25) %>% 
  filter(special_teams_result == "Return",
         return_start <= 0) %>% 
  group_by(return_start, starting_yardline) %>%
  summarise(count = n()) %>%
  # ggplot(., aes(x = return_start, y = starting_yardline, color = rev(count))) +
  ggplot(., aes(x = starting_yardline)) +
  geom_histogram(bins = 35, alpha = .75) +
  # coord_equal() + 
  scale_x_continuous(breaks = seq(-10, 100, 10)) + 
  scale_y_continuous(breaks = seq(0, 100, 10),  expand = c(0, .1)) + 
  geom_vline(xintercept = 25) +
  geom_vline(aes(xintercept = mean(starting_yardline, na.rm = T)), color = 'red', linetype = 'dashed') + 
  geom_vline(aes(xintercept = median(starting_yardline, na.rm = T)), color = 'green', linetype = 'dashed')


data$plays %>% 
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards)
         # game_id == "2018090600"
  ) %>% 
  mutate(
    return_start = 100 - (yardline_number + kick_length),
    return_type = case_when(
      special_teams_result == "Return" & return_start <= 0 ~ "endzone_return",
      special_teams_result == "Return" & return_start > 0 ~ "field_return",
      special_teams_result == "Touchback" ~ "touchback",
      TRUE ~ "FROG"),
    starting_yardline = case_when(
      special_teams_result == "Touchback" ~ 25,
      special_teams_result == "Return" ~ return_start + kick_return_yardage,
      TRUE ~ NA_real_),
    diff_from_default = starting_yardline - 25) %>%
  filter(return_type != "touchback") %>%
  ggplot(., aes(x = starting_yardline, fill = return_type)) + 
  geom_histogram(bins = 45, alpha = .95) + 
  facet_wrap(.~return_type, scales = "free") + 
  geom_vline(xintercept = 25) + 
  geom_vline(aes(xintercept = mean(starting_yardline))) + 
  scale_y_continuous(expand = c(0, 3))  + 
  theme_minimal() + 
  scale_fill_brewer(palette = 10) + 
  theme(legend.position = 'none')

data$plays %>% 
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards)
         # game_id == "2018090600"
  ) %>% 
  mutate(
    return_start = 100 - (yardline_number + kick_length),
    return_type = case_when(
      special_teams_result == "Return" & return_start <= 0 ~ "endzone_return",
      special_teams_result == "Return" & return_start > 0 ~ "field_return",
      special_teams_result == "Touchback" ~ "touchback",
      TRUE ~ "FROG"),
    starting_yardline = case_when(
      special_teams_result == "Touchback" ~ 25,
      special_teams_result == "Return" ~ return_start + kick_return_yardage,
      TRUE ~ NA_real_),
    diff_from_default = starting_yardline - 25) %>%
  filter(return_type != "touchback") %>%
  ggplot(., aes(x = return_type, y = starting_yardline, fill = return_type)) + 
  geom_boxplot(alpha = .75) + 
  coord_flip() + 
  geom_hline(yintercept = 25) +
  scale_y_continuous(expand = c(0, 3)) + 
  theme_minimal() + 
  scale_fill_brewer(palette = 2) + 
  theme(legend.position = 'none')

data$plays %>% 
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards)
         # game_id == "2018090600"
  ) %>% 
  mutate(
    return_start = 100 - (yardline_number + kick_length),
    return_type = case_when(
      special_teams_result == "Return" & return_start <= 0 ~ "endzone_return",
      special_teams_result == "Return" & return_start > 0 ~ "field_return",
      special_teams_result == "Touchback" ~ "touchback",
      TRUE ~ "FROG"),
    starting_yardline = case_when(
      special_teams_result == "Touchback" ~ 25,
      special_teams_result == "Return" ~ return_start + kick_return_yardage,
      TRUE ~ NA_real_),
    diff_from_default = starting_yardline - 25) %>%
  filter(return_type == "endzone_return") %>%
  ggplot(., aes(x = factor(return_start), y = starting_yardline, fill = factor(return_start))) +
  geom_boxplot() + 
  coord_flip() + 
  # facet_wrap(.~return_start, scales = "free") + 
  theme_minimal() + 
  # scale_fill_brewer(palette = 1) +
  theme(legend.position = 'none')

#------------------------------------------------
# game detail data ----

games_2018 <- nflfastR::load_pbp(2018)

data$plays %>% 
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards)
         # game_id == "2018090600"
  ) 

test_game <- (games_2018 %>% 
       filter(old_game_id == 2018090600)) %>%
  select(drive_play_id_started, everything()) 
view(test_game)

games_2018 %>% 
  group_by(fixed_drive_result) %>%
  tally() %>%
  View

games_2018 %>% 
  filter(old_game_id == "2018090600",
         fixed_drive_result == "Turnover on downs",
         !(desc %in% c("GAME"))
         # drive_start_transition == "KICKOFF"
         # play_id == "1368"
  ) %>% add_table()

games_2018 %>% 
  filter(
    # old_game_id == "2018090600",
    fixed_drive_result  != "End of half",
    !(desc %in% c("GAME")),
    !is.na(fixed_drive_result),
    is.na(drive_start_transition)
  ) %>% View


games_2018 %>% 
  filter(
    # old_game_id == "2018090600",
    fixed_drive_result  != "End of half",
    !(desc %in% c("GAME", "END GAME")),
    timeout == 0,
    !is.na(fixed_drive_result),
    drive_start_transition == "KICKOFF"
    # !is.na(fixed_drive_result)
  ) %>%  
  separate(drive_start_yard_line, into = c("drive_start_team_half", "drive_start_yard_line")) %>%
  select(old_game_id, home_team, away_team, posteam, drive_start_transition,
         drive_play_id_started, drive_start_team_half, drive_start_yard_line,
         drive_ended_with_score, fixed_drive_result) %>% 
  mutate(
    yards_to_endzone = case_when(
      posteam == drive_start_team_half ~ 100 - as.numeric(drive_start_yard_line),
      posteam != drive_start_team_half ~ 50 - as.numeric(drive_start_yard_line),
      TRUE ~ NA_real_),
    points_scored = case_when(
      fixed_drive_result == "Touchdown" ~ 6,
      fixed_drive_result == "Field goal" ~ 3,
      fixed_drive_result == "Safety" ~ 2,
      TRUE ~ 0),
    starting_yard_line_group = cut(yards_to_endzone, seq(0, 100, 10), right = FALSE)
  ) %>%
  left_join(., games_2018 %>% select(old_game_id, drive_play_id_started = play_id, touchback), by = c("old_game_id", "drive_play_id_started")) %>% 
  distinct() %>% 
  group_by(drive_start_transition, touchback) %>%
  summarise(count = n(),
            mean_success = mean(drive_ended_with_score),
            avg_points = mean(points_scored))


# check out home_wp_post; away_wp_post; epa; field material for punts adding weather data weather

# ideas: impact of S/T penalties on win prob, epa..
# cost/benefit of letting every punt bounce,
# score kickoff returns relative to 25 yardline (+ or -). model each addl yard away from 25 impact on win probability or epa