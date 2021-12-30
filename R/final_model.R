library(feather)
library(tidyverse)
library(purrr)
library(here)
library(tidymodels)
library(discrim)
library(baguette)
library(workflowsets)
library(finetune)
library(doParallel)
library(ggmosaic)
library(corrr)
library(vip)
library(ggthemr)

options(tidymodels.dark = TRUE) 

source(here::here("R", "util.R"))

#------------------------------------------------
# load data ----
kickoffs_model <- 
  read_feather(path = here("data", "model", "kickoffs_model.feather"))

#------------------------------------------------
# model_prep ----

# split data
set.seed(1504)
splits <- 
  rsample::initial_split(kickoffs_model, strata = return_type)
training <- 
  training(splits)
testing <- 
  testing(splits)

# create folds
folds <-
  rsample::bootstraps(training, times = 40, strata = return_type)

# define metrics
kickoff_metrics <- 
  metric_set(accuracy, roc_auc, mn_log_loss)

# specify recipe
recipe <- 
  recipe(return_type ~ ., data = training) %>%
  update_role(game_id, play_id, new_role = "ID Variable") %>%
  step_nzv(all_predictors()) %>%
  step_date(game_date, features = c("month", "dow")) %>%
  step_rm(game_date) %>%
  step_unknown(all_nominal_predictors(), -all_outcomes()) %>%
  step_novel(all_nominal_predictors(), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  themis::step_smote(all_outcomes(), skip = TRUE)

#------------------------------------------------
# specify model ----

rand_forest_spec <- 
  rand_forest(
    trees = 1000,
    min_n = tune(), 
    mtry = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

#------------------------------------------------
# get workflowset ----

update_mtry <-
  function(workflowset, workflow){
    
    workflowset %>%
      workflowsets::extract_workflow({{workflow}}) %>%
      parameters() %>%
      update(mtry = mtry(c(1, 20)))
  }

all_workflows <- 
  workflow_set(
    cross = TRUE,
    preproc = list(recipe = recipe),
    models = list(random_forest = rand_forest_spec)) %>%
  option_add(param_info = update_mtry(., "recipe_random_forest"), id = "recipe_random_forest")

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    allow_par = TRUE,
    parallel_over = "everything",
    verbose = TRUE
  )

#------------------------------------------------
# tune ----

cl <- 
  makeCluster(10)

doParallel::registerDoParallel(cl)

tictoc::tic()

rf_results <- 
  all_workflows %>%
  workflow_map(
    seed = 1503,
    control = grid_ctrl,
    fn = "tune_grid",
    resamples = folds,
    grid = 10,
    verbose = T,
    metrics = kickoff_metrics
  )

tictoc::toc()

train_metrics <- 
  rank_results(rf_results, "mn_log_loss", select_best = TRUE)

# #------------------------------------------------
# view model on test data ----

best_results <- 
  rf_results %>% 
  extract_workflow_set_result("recipe_random_forest") %>% 
  select_best(metric = "mn_log_loss")

test_results <- 
  rf_results %>% 
  extract_workflow("recipe_random_forest") %>% 
  finalize_workflow(best_results) %>% 
  last_fit(split = splits, 
           metrics = metric_set(mn_log_loss, roc_auc, accuracy))

test_metrics <-
  collect_metrics(test_results)

train_test_performance <- 
  train_metrics %>%
  select(.metric, value = mean) %>%
  mutate(type = "train") %>%
  bind_rows(test_metrics %>% select(.metric, value = .estimate) %>% mutate(type = "test"))

feature_importance <- 
  test_results %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20, alpha = .75, fill = "tomato") +
  theme_minimal() + 
  theme(text = add_big_labels()) + 
  scale_y_continuous(expand = c(0, 20)) 

ggsave(feature_importance, filename = here::here("img", "feature_importance.svg"))

pred_classes <- 
  bind_cols(test_results %>% 
            collect_predictions() %>% dplyr::select(.pred_class),
          testing(splits)) %>%
  dplyr::select(return_type, .pred_class) %>%
  group_by(return_type, .pred_class) 

caret::confusionMatrix(pred_classes$return_type, pred_classes$.pred_class)
write.csv(train_test_performance, file = here::here("model_data", "train_test_performance.csv"))
write.csv(pred_classes, file = here::here("model_data", "pred_classes.csv"))

stopCluster(cl)

# #------------------------------------------------
# save experiment ----

rf_model <-
  tibble(workflows = list(rf_results),
         splits = list(splits))

save_experiment(experiment = rf_model,
                workflow_name = "v1.0")
