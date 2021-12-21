
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

load_raw_data <- 
  function(file_name){
    feather::read_feather(here("data", "raw", {{file_name}}))
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
  map(., ~load_clean_data(.x)) %>%
  set_names(nm = tracking_file_names)

pbp <- 
  read_feather(path = here("data", "raw", "nflfastR_pbp_2010_2020.feather"))

#------------------------------------------------
# datasets ----

games <- 
  data$games

games %>% 
  add_table()

players <- 
  data$players

players %>% 
  add_table()

plays <- 
  data$plays

plays %>% 
  add_table()

scouting <- 
  data$PFFScoutingData 

scouting %>% 
  add_table()

tracking_2018 <-
  tracking_data$tracking2018 %>%
  janitor::clean_names() 

tracking_2019 <-
  tracking_data$tracking2019 %>%
  janitor::clean_names() 

tracking_2020 <-
  tracking_data$tracking2020 %>%
  janitor::clean_names() 

tracking <-
  bind_rows(tracking_2018, tracking_2019, tracking_2020)

tracking_2018 %>%
  janitor::clean_names() %>%
  filter(game_id == "2018090600",
         play_id == 677
         # display_name == "Foye Oluokun"
         ) %>%
  arrange(display_name, frame_id) %>%
  # sample_n(100) %>%
  add_table()

# tracking assign eahc player to home/away -> game_id, play_id -> plays -> game_id -> games to get home/away team names

player_team <- 
  tracking %>%
  filter(display_name != "football") %>%
  select(nfl_id, game_id, display_name, jersey_number, team) %>%
  distinct() %>%
  left_join(., games %>% select(game_id, season, ends_with("abbr")), by = c("game_id")) %>%
  mutate(player_team = ifelse(team == "home", home_team_abbr, visitor_team_abbr)) %>% 
  select(-ends_with("abbr"), -team)

tracking %>% 
  left_join(., player_team %>% 
              select(nfl_id, game_id, player_team), 
            by = c("nfl_id", "game_id")) %>%
  mutate(kickoff_event = ifelse(event %in% c("kickoff", "kick_received", "first_contact", "tackle"),
                                event,
                                NA_character_)) %>%
  tidyr::fill(kickoff_event, .direction = "down") %>% 
  filter(game_id == "2018090600",
         play_id == 677,
         nfl_id == 38707) %>% 
  View

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

# starting yardline for endzone returns
# data$plays %>% 
#   filter(special_teams_play_type == "Kickoff",
#          special_teams_result %in% c("Touchback", "Return"),
#          is.na(penalty_yards)
#          # game_id == "2018090600"
#   ) %>%  
#   mutate(
#     return_start = 100 - (yardline_number + kick_length),
#     starting_yardline = case_when(
#       special_teams_result == "Touchback" ~ 25,
#       special_teams_result == "Return" ~ return_start + kick_return_yardage,
#       TRUE ~ NA_real_),
#     diff_from_default = starting_yardline - 25) %>% 
#   filter(special_teams_result == "Return",
#          return_start <= 0) %>% 
#   group_by(return_start, starting_yardline) %>%
#   summarise(count = n()) %>%
#   # ggplot(., aes(x = return_start, y = starting_yardline, color = rev(count))) +
#   ggplot(., aes(x = starting_yardline)) +
#   geom_histogram(bins = 35, alpha = .75) +
#   scale_x_continuous(breaks = seq(-10, 100, 10)) + 
#   scale_y_continuous(breaks = seq(0, 100, 10),  expand = c(0, .1)) + 
#   geom_vline(xintercept = 25) +
#   geom_vline(aes(xintercept = mean(starting_yardline, na.rm = T)), color = 'red', linetype = 'dashed') + 
#   geom_vline(aes(xintercept = median(starting_yardline, na.rm = T)), color = 'green', linetype = 'dashed')
# 
# return_type_mean <- 
#   data$plays %>% 
#   filter(
#     special_teams_play_type == "Kickoff",
#     special_teams_result %in% c("Touchback", "Return"),
#     is.na(penalty_yards)) %>%
#   mutate(
#     return_start = 100 - (yardline_number + kick_length),
#     return_type = case_when(
#       special_teams_result == "Return" & return_start <= 0 ~ "Endzone Return",
#       special_teams_result == "Return" & return_start > 0 ~ "Field Return",
#       special_teams_result == "Touchback" ~ "Touchback",
#       TRUE ~ "FROG"),
#     starting_yardline = case_when(
#       special_teams_result == "Touchback" ~ 25,
#       special_teams_result == "Return" ~ return_start + kick_return_yardage,
#       TRUE ~ NA_real_)) %>%
#   group_by(return_type) %>%
#   summarise(mean = mean(starting_yardline, na.rm = T),
#             median = median(starting_yardline, na.rm = T))
#   
# # starting yardline for endzone returns
# data$plays %>% 
#   filter(special_teams_play_type == "Kickoff",
#          special_teams_result %in% c("Touchback", "Return"),
#          is.na(penalty_yards)) %>% 
#   mutate(
#     return_start = 100 - (yardline_number + kick_length),
#     return_type = case_when(
#       special_teams_result == "Return" & return_start <= 0 ~ "Endzone Return",
#       special_teams_result == "Return" & return_start > 0 ~ "Field Return",
#       special_teams_result == "Touchback" ~ "Touchback",
#       TRUE ~ "FROG"),
#     starting_yardline = case_when(
#       special_teams_result == "Touchback" ~ 25,
#       special_teams_result == "Return" ~ return_start + kick_return_yardage,
#       TRUE ~ NA_real_),
#     diff_from_default = starting_yardline - 25) %>%
#   filter(return_type != "touchback") %>%
#   left_join(., return_type_mean, by = "return_type") %>%
#   ggplot(., aes(x = starting_yardline, fill = return_type)) + 
#   geom_histogram(bins = 50, alpha = .75) + 
#   facet_wrap(.~return_type, scales = "free") + 
#   geom_vline(xintercept = 25) +
#   # geom_vline(aes(xintercept = mean), linetype = 'dashed') +
#   geom_vline(aes(xintercept = median), linetype = 'dashed') +
#   scale_y_continuous(expand = c(0, 1))  +
#   scale_x_continuous(breaks= seq(0, 100, 5)) + 
#   theme_minimal() + 
#   scale_fill_manual(values = c("cornflowerblue", "tomato")) +
#   theme(legend.position = 'none')

data$plays %>% 
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards)
         # game_id == "2018090600"
  ) %>% 
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
    diff_from_default = starting_yardline - 25)
  
  kickoffs_clean %>%
  filter(return_type != "Touchback") %>%
  ggplot(., aes(x = return_type, y = starting_yardline, fill = return_type)) + 
  geom_jitter(alpha = .3, color = 'gray') + 
  geom_boxplot(alpha = .75, outlier.shape = NA) + 
  coord_flip() + 
  geom_hline(yintercept = 25, linetype = 'dashed', color = "black") +
  # scale_y_continuous(expand = c(0, 3)) + 
  scale_y_continuous(breaks= seq(0, 100, 5)) + 
  theme_minimal() + 
  scale_fill_manual(values = c("cornflowerblue", "tomato")) + 
  theme(legend.position = 'none',
        panel.grid.minor = element_blank()) + 
  labs(x = "", y = "\nDrive Starting Yardline",
       title = "Running a kickoff out of the endzone tends to put the recieving team at a disadvantage")

data$plays %>% 
  left_join(., games %>% select(game_id, season), by = c("game_id")) %>%
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards)) %>% 
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
    diff_from_default = starting_yardline - 25)
  
  
  kickoffs_clean %>%
  group_by(return_type, season) %>%
  tally() %>%
  mutate(freq = n / sum(n),
         season = factor(season), levels = c("2018", "2019", "2020")) %>%
  ggplot(., aes(x = return_type, y = n, fill = season))  + 
  geom_col(position = "dodge", alpha = .75) + 
  theme_minimal() + 
  scale_y_continuous(expand = c(0, 10), breaks = seq(0, 1800, 50)) + 
  scale_fill_manual(values = c("cornflowerblue", "tomato", "orange")) + 
  labs(x = "", y = "Count", fill = "Season",
       title = "Despite the relative disadvantage, teams are more often running kickoffs out of the endzone",
       subtitle = "Are there certain game conditions tha make it more likely to return a kickoff out of the endzone?")

# data$plays %>% 
#   filter(special_teams_play_type == "Kickoff",
#          special_teams_result %in% c("Touchback", "Return"),
#          is.na(penalty_yards)
#          # game_id == "2018090600"
#   ) %>% 
#   mutate(
#     return_start = 100 - (yardline_number + kick_length),
#     return_type = case_when(
#       special_teams_result == "Return" & return_start <= 0 ~ "endzone_return",
#       special_teams_result == "Return" & return_start > 0 ~ "field_return",
#       special_teams_result == "Touchback" ~ "touchback",
#       TRUE ~ "FROG"),
#     starting_yardline = case_when(
#       special_teams_result == "Touchback" ~ 25,
#       special_teams_result == "Return" ~ return_start + kick_return_yardage,
#       TRUE ~ NA_real_),
#     diff_from_default = starting_yardline - 25) %>%
#   filter(return_type == "endzone_return") %>%
#   ggplot(., aes(x = factor(return_start), y = starting_yardline, fill = factor(return_start))) +
#   geom_boxplot() + 
#   coord_flip() + 
#   # facet_wrap(.~return_start, scales = "free") + 
#   theme_minimal() + 
#   # scale_fill_brewer(palette = 1) +
#   theme(legend.position = 'none')

#------------------------------------------------
# game pbp detail data ----

games_2018 <-
  nflfastR::load_pbp(2018)

pbp %>%
  filter(kickoff_attempt == 1,
         season >= 2011) %>%
  group_by(season, touchback) %>%
  tally() %>% 
  mutate(freq = n / sum(n)) %>% 
  ggplot(., aes(x = factor(season), group = touchback, y = freq, color = factor(touchback))) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1), labels = scales::percent) + 
  labs(x = "Season", y = "", title = "More touchbacks are occuring")


kickoff_dataset <- 
  data$plays %>% 
  left_join(., games %>% select(game_id, season), by = c("game_id")) %>%
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards)) %>% 
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
    diff_from_default = starting_yardline - 25)

kickoffs_clean %>% 
  left_join(., pbp %>% 
              select(old_game_id, fixed_drive_result) %>%
              mutate(old_game_id = as.numeric(old_game_id)),
            by = c("game_id" = "old_game_id")) %>%
  filter(fixed_drive_result != "End of half",
         !is.na(fixed_drive_result),
         return_type != "Field Return") %>%
  group_by(return_type, fixed_drive_result) %>% 
  tally() %>%
  ungroup() %>% 
  group_by(return_type) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  ggplot(., aes(x = fixed_drive_result, y = freq, fill = return_type))  + 
  geom_col(position = "dodge", alpha = .75) + 
  theme_minimal() + 
  # facet_wrap(.~return_type) + 
  coord_flip() + 
  geom_text(aes(label = paste(freq*100, "%")), position = position_dodge(width = .9), hjust = -.2) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, .35), breaks = seq(0, 1, .05), labels = scales::percent) +
  scale_fill_manual(values = c("cornflowerblue", "tomato", "orange"))

kickoffs_clean %>% 
  left_join(., pbp %>% 
              select(old_game_id, epa) %>%
              mutate(old_game_id = as.numeric(old_game_id)),
            by = c("game_id" = "old_game_id")) %>%
  filter(return_type != "Field Return") %>%
  group_by(return_type) %>%
  summarise(mean_epa = mean(epa, na.rm  = T)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  ggplot(., aes(x = return_type, y = mean_epa, fill = return_type))  + 
  geom_col(position = "dodge", alpha = .75) + 
  theme_minimal() + 
  # facet_wrap(.~return_type) + 
  coord_flip() + 
  # geom_text(aes(label = paste(freq*100, "%")), position = position_dodge(width = .9), hjust = -.2) + 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, .35), breaks = seq(0, 1, .05), labels = scales::percent) +
  scale_fill_manual(values = c("cornflowerblue", "tomato", "orange"))

pbp %>%
  filter(kickoff_attempt == 1,
         season >= 2011
         # !(is.na(kickoff_returner_player_id)),
           # kickoff_in_endzone == 1
         ) %>%
  # select(season, kickoff_returner_player_id, kickoff_in_endzone, drive_start_yard_line) %>% 
  group_by(season, touchback) %>%
  tally() %>% 
  mutate(freq = n / sum(n)) %>% 
  ggplot(., aes(x = factor(season), group = touchback, y = freq, color = factor(touchback))) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1), labels = scales::percent) + 
  labs(x = "Season", y = "", title = "Mean Touchbacks per Season")


  data$plays %>% 
  filter(special_teams_play_type == "Kickoff",
         special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards)
         # game_id == "2018090600"
  ) 

test_game <- 
  games_2018 %>% 
  filter(old_game_id == 2018090600
         # play_id == 1387
         ) %>%
  select(drive_play_id_started, everything()) %>%
  select(where(~!all(is.na(.x))))

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