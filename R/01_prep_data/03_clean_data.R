
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
  c("games", "PFFScoutingData", "players", "plays")

# read in .feather files, excluding tracking
data <- 
  setdiff(list.files(path = here("data", "raw"), pattern = "_raw.feather"),
          list.files(path = here("data", "raw"), pattern = "tracking")) %>%
  map(., ~load_data(.x, type = "raw")) %>% 
  set_names(nm = file_names)

data %>%
  map(glimpse)

# load tracking data
tracking_file_names <-
  c("tracking2018", "tracking2019", "tracking2020")

# read in .feather files
tracking_data <-
  setdiff(list.files(path = here("data", "raw"), pattern = "tracking"),
          list.files(path = here("data", "raw"), pattern = ".csv")) %>%
  map(., ~load_data(.x, type = "raw")) %>%
  set_names(nm = tracking_file_names)

tracking_data %>%
  map(glimpse)

#------------------------------------------------
# clean games  ----

games_raw <- 
  data$games %>%
  janitor::clean_names()

skim(games_raw)
str(games_raw)

# add holidays; could be interesting to see if holidays have impact on return behavior
holidays <- 
  c("USChristmasDay", "USColumbusDay", "USInaugurationDay", "USLaborDay", "USThanksgivingDay", "USMLKingsBirthday")

holiday_lookup <-
  map(holidays, ~timeDate::holiday(year = 2017:2022, Holiday = .x)) %>%
  set_names(nm = holidays) %>%
  as.data.frame()

names(holiday_lookup) <- holidays

holiday_lookup <- 
  holiday_lookup %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = 1:6) %>% 
  rename(holiday = name, date = value) %>%
  mutate(holiday = str_replace(holiday, "US", "")) %>%
  select(-id)

games <- 
  games_raw %>%
  mutate(game_date = mdy(game_date),
         game_datetime = ymd_hms(paste(game_date, game_time_eastern)),
         game_hour = hour(game_time_eastern),
         game_tod = case_when(
           hour(game_time_eastern) == 9 ~ 'morning',
           hour(game_time_eastern) %in% c(12, 13, 15) ~ 'afternoon',
           hour(game_time_eastern) %in% c(16, 17) ~ 'late_afternoon',
           hour(game_time_eastern) %in% c(19, 20, 21, 22) ~ 'night',
           TRUE ~ "FROG"),
         day_nm = wday(game_date, label = T, abbr = T),
         month = month(game_date, label = T, abbr = T),
         primetime_ind = ifelse(game_tod == 'night', 1, 0),
         overseas_game_ind = ifelse(game_time_eastern == 9, 1, 0)) %>%
  left_join(., holiday_lookup, by = c("game_date" = "date")) %>%
  mutate(holiday = ifelse(is.na(holiday), 'none', holiday))

games %>%
  add_table()

#------------------------------------------------
# clean players ----

players_raw <- 
  data$players %>%
  janitor::clean_names()

skim(data$players_raw)

players <-
  players_raw %>%
  tidyr::separate(height, into = c("feet", "inches")) %>%
  mutate(feet = as.numeric(feet),
         inches = as.numeric(inches),
         inches_2 = ifelse(feet > 10, feet, NA_real_),
         inches_3 = ifelse(feet < 10, feet*12 + inches, NA_real_),
         birth_date = as.Date(birth_date),
         current_age = as.numeric((Sys.Date() - birth_date)/365),
         inches_new = coalesce(inches_2, inches_3)) %>%
  select(-c(feet, inches, inches_2, inches_3)) %>% 
  select(nfl_id, inches = inches_new, everything())

players %>%
  add_table()

#------------------------------------------------
# clean plays ----
# contains stat on every special teams play
plays_raw <- 
  data$plays %>%
  janitor::clean_names()

skim(plays_raw)

plays <- 
  plays_raw 

plays %>%
  add_table()

#------------------------------------------------
# clean scouting ----
# contains a scouting report for every special teams play 
# data dicitonary at: https://www.kaggle.com/c/nfl-big-data-bowl-2022/data

scouting_raw <- 
  data$PFFScoutingData %>%
  janitor::clean_names()

skim(scouting_raw)

scouting <- 
  scouting_raw %>%
  mutate(
    # can be blank for a field goal or kickoff
    snap_detail = case_when(
      snap_detail == "H" ~ "high",
      snap_detail == "L" ~ "low",
      snap_detail == "<" ~ "left",
      snap_detail == ">" ~ "right",
      TRUE ~ snap_detail),
    kick_type = case_when(
      # can be blank for a field goal or kickoff
      kick_type == "D" ~ "deep",
      kick_type == "F" ~ "flat",
      kick_type == "K" ~ "free_kick",
      kick_type == "O" ~ "obvious_onside",
      kick_type == "P" ~ "pooch",
      kick_type == "Q" ~ "squib",
      kick_type == "S" ~ "surprise_onside",
      kick_type == "B" ~ "deep_direct_oob",
      kick_type == "N" ~ "normal",
      kick_type == "R" ~ "rugby",
      kick_type == "A" ~ "nose_down_aussie",
      is.na(kick_type) ~ NA_character_,
      TRUE ~ "FROG"),
    kick_contact_type = case_when(
      # can be blank for a field goal or kickoff
      kick_contact_type == "BB" ~ "bounced_backwards",
      kick_contact_type == "BC" ~ "bobbled_catch_from_air",
      kick_contact_type == "BF" ~ "bounced_forwards",
      kick_contact_type == "BOG" ~ "bobbled_on_ground",
      kick_contact_type == "CC" ~ "clean_catch_from_air",
      kick_contact_type == "CFFG" ~ "clean_field_from_ground",
      kick_contact_type == "DEZ" ~ "direct_to_endzone",
      kick_contact_type == "ICC" ~ "incidental_coverage_team_contact",
      kick_contact_type == "KTB" ~ "kick_team_knocked_back",
      kick_contact_type == "KTC" ~ "kick_team_catch",
      kick_contact_type == "KTF" ~ "kick_team_knocked_forward",
      kick_contact_type == "MBC" ~ "muffed_by_contact_non_designated_returner",
      kick_contact_type == "MBDR" ~ "muffed_by_designated_returner",
      kick_contact_type == "OOB" ~ "directly_out_of_bounds",
      is.na(kick_contact_type) ~ NA_character_,
      TRUE ~ "FROG")
  )

scouting %>%
  add_table()

#------------------------------------------------
# clean tracking data ----

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

#------------------------------------------------
# save cleaned data ----

write_feather(x = games, path = here("data", "clean", "games_clean.feather"))
write_feather(x = players, path = here("data", "clean", "players_clean.feather"))
write_feather(x = plays, path = here("data", "clean", "plays_clean.feather"))
write_feather(x = scouting, path = here("data", "clean", "scouting_clean.feather"))
write_feather(x = tracking, path = here("data", "clean", "tracking.feather"))