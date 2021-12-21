

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
library(tidymodels)
library(discrim)
library(workflowsets)
library(doParallel)

options(tidymodels.dark = TRUE) 

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

nflfastR_data <- 
  read_feather(path = here("data", "raw", "nflfastR_data.feather"))

#------------------------------------------------
# kickoff data ----
kickoffs <- 
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
      TRUE ~ "FROG")
  ) %>%
  filter(return_type %in% c("endzone_return", "touchback")) %>%
  left_join(., data$games %>% mutate(teams = paste(home_team_abbr, visitor_team_abbr, sep = " "),
                                     game_tod = case_when(
                                       hour(game_time_eastern) == 9 ~ 'morning',
                                       hour(game_time_eastern) %in% c(12, 13, 15) ~ 'afternoon',
                                       hour(game_time_eastern) %in% c(16, 17) ~ 'late_afternoon',
                                       hour(game_time_eastern) == 20 ~ 'night',
                                       hour(game_time_eastern) == 22 ~ 'late_night',
                                       TRUE ~ "FROG")
                                     ), by = c("game_id")) %>% 
  rowwise() %>%
  mutate(recieving_team = ifelse(trimws(str_remove(teams, possession_team)) == trimws(visitor_team_abbr), "visiting_team", "home_team"),
         recieving_team_score_diff = ifelse(recieving_team == visitor_team_abbr,
                                            pre_snap_visitor_score - pre_snap_home_score, 
                                            pre_snap_home_score - pre_snap_visitor_score)) %>%
  select(-c(down, yards_to_go, possession_team, special_teams_play_type, kicker_id, kick_blocker_id, yardline_side, 
            yardline_number, starts_with("penalty"), starts_with("pre"), pass_result, kick_length, kick_return_yardage, play_result, 
            absolute_yardline_number, teams, special_teams_result, play_description)) %>%
  # left_join(., data$players %>% select(-c(display_name)), by = c("returner_id" = "nfl_id")) %>%
  # mutate(game_day_age = as.numeric(as.Date(game_date) - birth_date)/365) %>%
  # select(-c(birth_date, current_age)) %>%
  left_join(., nflfastR_data %>% 
              mutate(old_game_id = as.numeric(old_game_id)) %>%
              select(old_game_id, play_id, half_seconds_remaining, game_seconds_remaining, game_half, 
                     home_timeouts_remaining, away_timeouts_remaining, 
                     temp, wind, surface), by = c("game_id" = "old_game_id", "play_id" = "play_id")) %>% 
  mutate(recieving_team_timeouts_remaining = ifelse(recieving_team == visitor_team_abbr, away_timeouts_remaining, home_timeouts_remaining)) %>%
  select(-c(home_timeouts_remaining, away_timeouts_remaining)) %>%
  distinct()

# kickoffs %>% add_table()

#------------------------------------------------
# model_prep ----

kickoffs <- 
  kickoffs %>%
  select(-c(game_clock, game_time_eastern, home_team_abbr, visitor_team_abbr, game_datetime, recieving_team, temp, wind, returner_id)) %>%
  mutate(return_type = factor(return_type, levels = c("endzone_return", "touchback")),
         game_date = as.Date(game_date),
         quarter = as.factor(quarter),
         season = as.factor(season),
         game_tod = as.factor(game_tod),
         primetime_ind = as.factor(primetime_ind),
         overseas_game_ind = as.factor(overseas_game_ind),
         week = as.factor(week),
         game_hour = as.factor(game_hour),
         recieving_team_timeouts_remaining = as.factor(recieving_team_timeouts_remaining),
         holiday = as.factor(holiday),
         half_mins_remaining = half_seconds_remaining / 60,
         game_minsremaining = game_seconds_remaining / 60,
         game_half = as.factor(game_half),
         quarter = as.factor(quarter),
         season = as.factor(season),
         surface = as.factor(surface)
  ) %>%
  select(-c(half_seconds_remaining, game_seconds_remaining)) %>%
  ungroup() 

# str(kickoffs)
# skimr::skim_to_list(kickoffs)

set.seed(1504)
splits <- 
  rsample::initial_split(kickoffs, strata = return_type)
training <- 
  training(splits)
testing <- 
  testing(splits)

folds <-
  rsample::bootstraps(training, times = 40, strata = return_type)

folds <-
  rsample::vfold_cv(training, v = 20, strata = return_type)

recipe <- 
  recipe(return_type ~ ., data = training) %>%
  update_role(game_id, play_id, new_role = "ID Variable") %>%
  step_nzv(all_predictors(), -game_tod) %>%
  step_date(game_date, features = c("month", "dow")) %>%
  step_rm(game_date) %>%
  # step_unknown(all_nominal_predictors(), -all_outcomes()) %>%
  # step_novel(all_nominal_predictors(), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric_predictors(),  -month, -day_nm) %>%
  step_normalize(all_numeric_predictors(), -month, -day_nm) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  themis::step_smote(all_outcomes(), skip = TRUE)

# juice(prep(recipe)) %>% View

#------------------------------------------------
# model ----
glm_spec <- 
  logistic_reg(penalty = tune(),
               mixture = tune()) %>% 
  set_engine("glm")

tree_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  )%>% 
  set_engine("rpart") %>% 
  set_mode("classification")

mars_spec <- 
  mars(
    num_terms = tune(),
    prod_degree = tune(),
    prune_method = tune()) %>% 
  set_engine("earth") %>% 
  set_mode("classification")

mars_disc_spec <- 
  discrim_flexible(prod_degree = tune()) %>% 
  set_engine("earth")

cart_spec <- 
  decision_tree(cost_complexity = tune(), 
                min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

mlp_spec <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    dropout = tune(),
    epochs = tune(),
    activation = tune()) %>% 
  set_engine("nnet") %>% 
  set_mode("classification")

bayes_spec <- 
  naive_Bayes(
    smoothness = tune(),
    Laplace = tune()) %>% 
  set_engine("klaR") %>% 
  set_mode("classification")

svm_spec <- 
  svm_rbf(
    cost = tune(),
    rbf_sigma = tune(),
    margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

rand_forest_spec <- 
  rand_forest(
    trees = 1000,
    min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

all_workflows <- 
  workflow_set(
    cross = TRUE,
    preproc = list(recipe = recipe),
    models = list(mars = mars_spec, 
                  mars_discrim = mars_disc_spec,
                  decision_tree = tree_spec,
                  glm = glm_spec,
                  cart = cart_spec,
                  # nnet = mlp_spec,
                  naive_bayes = bayes_spec
                  # svm = svm_spec,
                  # random_forest = rand_forest_spec
    )
  )

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    allow_par = TRUE,
    parallel_over = "everything",
    verbose = TRUE
  )

cl <- 
  makeCluster(10)

doParallel::registerDoParallel(cl)

results <- 
  all_workflows %>%
  workflow_map(
    seed = 1503,
    resamples = folds,
    grid = 25,
    verbose = T,
    metrics = metric_set(accuracy, spec, sens, roc_auc),
    control = grid_ctrl
  )


stopCluster(cl)

workflowsets::rank_results(results, rank_metric = "roc_auc", select_best = T)

autoplot(
  results,
  rank_metric = "roc_auc",  # <- how to order models
  metric = "roc_auc",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
)

sautoplot(
  results  %>% filter(!wflow_id %in% c("recipe_svm", "recipe_random_forest")),
  rank_metric = "accuracy",  # <- how to order models
  metric = "accuracy",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
)

results %>% unnest(result) %>% unnest(.notes) %>% select(wflow_id, .notes)
