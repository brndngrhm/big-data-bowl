
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

# team logos
url_logo <- 
  getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df_logos <- 
  read.csv(text = url_logo)

#------------------------------------------------
# focus on kickoffs ----

# starting yardline histogram
return_type_mean <-
  kickoffs %>%
  group_by(return_type) %>%
  summarise(mean = mean(starting_yardline, na.rm = T),
            median = median(starting_yardline, na.rm = T))

kickoffs %>%
  filter(return_type != "Touchback") %>%
  left_join(., return_type_mean, by = "return_type") %>%
  ggplot(., aes(x = starting_yardline, fill = return_type)) +
  geom_histogram(bins = 50, alpha = .75) +
  facet_wrap(.~return_type, scales = "free") +
  geom_vline(xintercept = 25) +
  geom_vline(aes(xintercept = median), linetype = 'dashed') +
  scale_y_continuous(expand = c(0, 1))  +
  scale_x_continuous(breaks= seq(0, 100, 5)) +
  theme_minimal() +
  scale_fill_manual(values = c("cornflowerblue", "tomato")) +
  theme(legend.position = 'none')

# Starting yardline boxplot
kickoffs %>%
  filter(return_type != "Touchback") %>%
  ggplot(., aes(x = return_type, y = starting_yardline, fill = return_type)) + 
  geom_jitter(alpha = .3, color = 'gray') + 
  geom_boxplot(alpha = .75, outlier.shape = NA) + 
  coord_flip() + 
  geom_hline(yintercept = 25, linetype = 'dashed', color = "black") +
  scale_y_continuous(breaks= seq(0, 100, 5)) + 
  theme_minimal() + 
  scale_fill_manual(values = c("cornflowerblue", "tomato")) + 
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        text = add_big_labels()) + 
  labs(x = "", y = "\nDrive Starting Yardline",
       title = "Running a kickoff out of the endzone tends to put the recieving team at a disadvantage")

# Count of return types by season
kickoffs %>%
  group_by(return_type, season) %>%
  tally() %>%
  mutate(freq = n / sum(n),
         season = factor(season), levels = c("2018", "2019", "2020")) %>%
  ggplot(., aes(x = return_type, y = n, fill = season))  + 
  geom_col(position = "dodge", alpha = .75) + 
  theme_minimal() + 
  theme(text = add_big_labels(text_size = 14)) + 
  scale_y_continuous(expand = c(0, 10), breaks = seq(0, 1800, 50)) + 
  scale_fill_manual(values = c("cornflowerblue", "tomato", "orange")) + 
  labs(x = "", y = "Count", fill = "Season",
       title = "Despite the relative disadvantage, teams are more often running kickoffs out of the endzone",
       subtitle = "Are there certain game conditions tha make it more likely to return a kickoff out of the endzone?")

# starting yardline vs endzone kick depth
kickoffs %>%
  filter(return_type == "Endzone Return") %>%
  ggplot(., aes(x = factor(return_start), y = starting_yardline)) +
  geom_jitter(alpha = .3, color = 'gray') + 
  geom_boxplot(alpha = .75, outlier.shape = NA, fill = "cornflowerblue") + 
  coord_flip() +
  geom_hline(yintercept = 25, linetype = 'dashed', color = "black") +
  theme_minimal() +
  theme(text = big_labels()) + 
  theme(legend.position = 'none') +
  labs(x = "Kickoff Yards into Endzone", y = "Drive Starting Yardline",
       title = "Fewer kickoffs are returned from Deep in the Endzone",
       subtitle = "The ones that are tend to lead to drives that start with worse field position")

# kickoff hangtime vs starting yardline
kickoffs %>%
  filter(return_type != "Touchback") %>%
  left_join(., scouting_kickoff, by = c("game_id", "play_id")) %>%
  ggplot(., aes(x = starting_yardline, y = hang_time, color = return_type)) +
  geom_jitter(alpha = .3, size = 2.1) + 
  coord_flip() + 
  geom_vline(xintercept = 25, linetype = 'dashed', color = "black") +
  theme_minimal() +
  theme(text = big_labels()) + 
  # theme(legend.position = 'none') +
  scale_color_manual(values = c("cornflowerblue", "tomato")) + 
  labs(y = "Kickoff Hangtime (seconds)", x = "Drive Starting Yardline", color = "",
       title = "Kickoff hangtime not related to starting yardline",
       subtitle = "Excludes touchbacks")

# return type vs EPA
kickoffs %>%
  left_join(., pbp_kickoff %>% 
              select(old_game_id, play_id, epa) %>% 
              distinct(), by = c("game_id" = "old_game_id", "play_id")) %>%
  filter(return_type != "Touchback",
         between(epa, -4, 4)) %>%
  ggplot(., aes(x = return_type, y = epa, fill = return_type)) +
  geom_jitter(alpha = .3, color = 'gray') + 
  geom_boxplot(alpha = .75, outlier.shape = NA) + 
  scale_y_continuous(breaks = seq(-4, 4, .5)) +
  coord_flip() +
  theme_minimal() +
  theme(text = big_labels()) + 
  theme(legend.position = 'none') +
  scale_fill_manual(values = c("cornflowerblue", "tomato")) + 
  labs(x = "", y = "EPA",
       title = "Endzone returns reduce EPA for the recieving team")

# starting yardline vs endzone kick depth
kickoffs %>%
  filter(return_type == "Endzone Return") %>%
  group_by(game_id, recieving_team_name) %>%
  summarise(n = n(),
            yards_gained = sum(kick_return_yardage, na.rm = T),
            yds_per_return = yards_gained/n) %>%
  ungroup() %>%
  group_by(recieving_team_name) %>%
  mutate(mean_n = mean(n),
         mean_yards = mean(yds_per_return)) %>% 
  ungroup() %>%
  mutate(overall_mean_n = mean(n),
         overall_mean_yards = mean(mean_yards)) %>%
  left_join(., df_logos, by = c("recieving_team_name" = "team_code")) %>%
  ggplot(., aes(x = mean_yards, y = mean_n)) +
  geom_image(aes(image = url), size = 0.04) + 
  # scale_x_continuous(breaks = seq(.5, 2.5, .1)) +
  geom_hline(aes(yintercept = overall_mean_n), linetype = 'dashed') +
  geom_vline(aes(xintercept = overall_mean_yards), linetype = 'dashed') +
  theme_minimal() +
  theme(text = big_labels()) +
  theme(legend.position = 'none') +
  labs(y = "Avg Endzone Returns per Game\n", x = "\nAvg Return Yards",
       title = "Endzone Kickoff Returns 2018-2020")

#------------------------------------------------
# game pbp detail data ----
# 
# games_2018 <-
#   nflfastR::load_pbp(2018)
# 
# pbp %>%
#   filter(kickoff_attempt == 1,
#          season >= 2011) %>%
#   group_by(season, touchback) %>%
#   tally() %>% 
#   mutate(freq = n / sum(n)) %>% 
#   ggplot(., aes(x = factor(season), group = touchback, y = freq, color = factor(touchback))) + 
#   geom_line() + 
#   geom_point() + 
#   scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1), labels = scales::percent) + 
#   labs(x = "Season", y = "", title = "More touchbacks are occuring")
# 
# 
# kickoff_dataset <- 
#   data$plays %>% 
#   left_join(., games %>% select(game_id, season), by = c("game_id")) %>%
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
#     diff_from_default = starting_yardline - 25)
# 
# kickoffs_clean %>% 
#   left_join(., pbp %>% 
#               select(old_game_id, fixed_drive_result) %>%
#               mutate(old_game_id = as.numeric(old_game_id)),
#             by = c("game_id" = "old_game_id")) %>%
#   filter(fixed_drive_result != "End of half",
#          !is.na(fixed_drive_result),
#          return_type != "Field Return") %>%
#   group_by(return_type, fixed_drive_result) %>% 
#   tally() %>%
#   ungroup() %>% 
#   group_by(return_type) %>%
#   mutate(freq = n / sum(n)) %>%
#   mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
#   ggplot(., aes(x = fixed_drive_result, y = freq, fill = return_type))  + 
#   geom_col(position = "dodge", alpha = .75) + 
#   theme_minimal() + 
#   # facet_wrap(.~return_type) + 
#   coord_flip() + 
#   geom_text(aes(label = paste(freq*100, "%")), position = position_dodge(width = .9), hjust = -.2) + 
#   scale_y_continuous(expand = c(0, 0), limits = c(0, .35), breaks = seq(0, 1, .05), labels = scales::percent) +
#   scale_fill_manual(values = c("cornflowerblue", "tomato", "orange"))
# 
# kickoffs_clean %>% 
#   left_join(., pbp %>% 
#               select(old_game_id, epa) %>%
#               mutate(old_game_id = as.numeric(old_game_id)),
#             by = c("game_id" = "old_game_id")) %>%
#   filter(return_type != "Field Return") %>%
#   group_by(return_type) %>%
#   summarise(mean_epa = mean(epa, na.rm  = T)) %>% 
#   mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
#   ggplot(., aes(x = return_type, y = mean_epa, fill = return_type))  + 
#   geom_col(position = "dodge", alpha = .75) + 
#   theme_minimal() + 
#   # facet_wrap(.~return_type) + 
#   coord_flip() + 
#   # geom_text(aes(label = paste(freq*100, "%")), position = position_dodge(width = .9), hjust = -.2) + 
#   # scale_y_continuous(expand = c(0, 0), limits = c(0, .35), breaks = seq(0, 1, .05), labels = scales::percent) +
#   scale_fill_manual(values = c("cornflowerblue", "tomato", "orange"))
# 
# pbp %>%
#   filter(kickoff_attempt == 1,
#          season >= 2011
#          # !(is.na(kickoff_returner_player_id)),
#            # kickoff_in_endzone == 1
#          ) %>%
#   # select(season, kickoff_returner_player_id, kickoff_in_endzone, drive_start_yard_line) %>% 
#   group_by(season, touchback) %>%
#   tally() %>% 
#   mutate(freq = n / sum(n)) %>% 
#   ggplot(., aes(x = factor(season), group = touchback, y = freq, color = factor(touchback))) + 
#   geom_line() + 
#   geom_point() + 
#   scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1), labels = scales::percent) + 
#   labs(x = "Season", y = "", title = "Mean Touchbacks per Season")
# 
# 
#   data$plays %>% 
#   filter(special_teams_play_type == "Kickoff",
#          special_teams_result %in% c("Touchback", "Return"),
#          is.na(penalty_yards)
#          # game_id == "2018090600"
#   ) 
# 
# test_game <- 
#   games_2018 %>% 
#   filter(old_game_id == 2018090600
#          # play_id == 1387
#          ) %>%
#   select(drive_play_id_started, everything()) %>%
#   select(where(~!all(is.na(.x))))
# 
# view(test_game)
# 
# games_2018 %>% 
#   group_by(fixed_drive_result) %>%
#   tally() %>%
#   View
# 
# games_2018 %>% 
#   filter(old_game_id == "2018090600",
#          fixed_drive_result == "Turnover on downs",
#          !(desc %in% c("GAME"))
#          # drive_start_transition == "KICKOFF"
#          # play_id == "1368"
#   ) %>% add_table()
# 
# games_2018 %>% 
#   filter(
#     # old_game_id == "2018090600",
#     fixed_drive_result  != "End of half",
#     !(desc %in% c("GAME")),
#     !is.na(fixed_drive_result),
#     is.na(drive_start_transition)
#   ) %>% View
# 
# 
# games_2018 %>% 
#   filter(
#     # old_game_id == "2018090600",
#     fixed_drive_result  != "End of half",
#     !(desc %in% c("GAME", "END GAME")),
#     timeout == 0,
#     !is.na(fixed_drive_result),
#     drive_start_transition == "KICKOFF"
#     # !is.na(fixed_drive_result)
#   ) %>%  
#   separate(drive_start_yard_line, into = c("drive_start_team_half", "drive_start_yard_line")) %>%
#   select(old_game_id, home_team, away_team, posteam, drive_start_transition,
#          drive_play_id_started, drive_start_team_half, drive_start_yard_line,
#          drive_ended_with_score, fixed_drive_result) %>% 
#   mutate(
#     yards_to_endzone = case_when(
#       posteam == drive_start_team_half ~ 100 - as.numeric(drive_start_yard_line),
#       posteam != drive_start_team_half ~ 50 - as.numeric(drive_start_yard_line),
#       TRUE ~ NA_real_),
#     points_scored = case_when(
#       fixed_drive_result == "Touchdown" ~ 6,
#       fixed_drive_result == "Field goal" ~ 3,
#       fixed_drive_result == "Safety" ~ 2,
#       TRUE ~ 0),
#     starting_yard_line_group = cut(yards_to_endzone, seq(0, 100, 10), right = FALSE)
#   ) %>%
#   left_join(., games_2018 %>% select(old_game_id, drive_play_id_started = play_id, touchback), by = c("old_game_id", "drive_play_id_started")) %>% 
#   distinct() %>% 
#   group_by(drive_start_transition, touchback) %>%
#   summarise(count = n(),
#             mean_success = mean(drive_ended_with_score),
#             avg_points = mean(points_scored))


# check out home_wp_post; away_wp_post; epa; field material for punts adding weather data weather

# ideas: impact of S/T penalties on win prob, epa..
# cost/benefit of letting every punt bounce,
# score kickoff returns relative to 25 yardline (+ or -). model each addl yard away from 25 impact on win probability or epa