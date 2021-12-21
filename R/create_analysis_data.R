
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

load_clean_data <- 
  function(file_name){
    feather::read_feather(here("data", "clean", {{file_name}}))
  }

#------------------------------------------------
# load data ----
file_names <- 
  c("games", "players", "plays", "PFFScoutingData")

data <- 
  setdiff(list.files(path = here("data", "clean"), pattern = ".feather"),
          list.files(path = here("data", "clean"), pattern = "tracking")) %>%
  map(., ~load_clean_data(.x)) %>% 
  set_names(nm = file_names)

tracking_raw <-
  feather::read_feather(here("data", "clean", "tracking.feather"))

# play by play data from NFLFastR
pbp <- 
  read_feather(path = here("data", "raw", "nflfastR_data.feather"))

games <- 
  data$games

players <- 
  data$players

plays <- 
  data$plays

scouting <- 
  data$PFFScoutingData 

remove(data)

#------------------------------------------------
# kickoff data ----

# all kickoffs, with some additional variables added around the type and length of return
kickoffs_all <- 
  data$plays %>% 
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
  left_join(., data$games %>% 
              mutate(teams = paste(home_team_abbr, visitor_team_abbr, sep = " "),
                     game_tod = case_when(
                       hour(game_time_eastern) == 9 ~ 'morning',
                       hour(game_time_eastern) %in% c(12, 13, 15) ~ 'afternoon',
                       hour(game_time_eastern) %in% c(16, 17) ~ 'late_afternoon',
                       hour(game_time_eastern) == 20 ~ 'night',
                       hour(game_time_eastern) == 22 ~ 'late_night',
                       TRUE ~ "FROG")), by = c("game_id")) %>%
  rowwise() %>%
  mutate(recieving_team = ifelse(trimws(str_remove(teams, possession_team)) == trimws(visitor_team_abbr), "visiting_team", "home_team"),
         recieving_team_score_diff = ifelse(recieving_team == visitor_team_abbr,
                                            pre_snap_visitor_score - pre_snap_home_score, 
                                            pre_snap_home_score - pre_snap_visitor_score)) 

# kickoffs without fumbles, OOB,... only returns or touchbacks, no penalties
kickoffs_clean <- 
  kickoffs_all %>% 
  filter(special_teams_result %in% c("Touchback", "Return"),
         is.na(penalty_yards))

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
# tracking data

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
  tally(sort = T, name = "Kickoff_event_sequences") %>%
  mutate(freq = Kickoff_event_sequences / sum(Kickoff_event_sequences))
  
kickoff_tracking %>%
  select(game_id, play_id, event) %>%
  distinct() %>%
  group_by(event) %>% 
  tally() %>% View

kickoff_tracking %>% 
  filter(event == "kickoff_play") %>% View 

#------------------------------------------------
# scounting data

scouting_kickoff <- 
  scouting %>%
  mutate(tracking_kickoff_id = paste0(game_id, play_id)) %>%
  filter(tracking_kickoff_id %in% kickoff_ids)

remove(scouting)

#------------------------------------------------
# play by play data

pbp_kickoff <- 
  pbp %>%
  mutate(old_game_id = as.numeric(old_game_id),
         tracking_kickoff_id = paste0(old_game_id, play_id)) %>%
  filter(tracking_kickoff_id %in% kickoff_ids)

remove(pbp)


