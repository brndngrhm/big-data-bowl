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

options(tidymodels.dark = TRUE) 

source(here("R", "util.R"))

#------------------------------------------------
# load data ----
kickoffs_model <- 
  read_feather(path = here("data", "model", "kickoffs_model.feather"))

# kickoffs_model %>% add_table()
# 
# str(kickoffs_model)

#------------------------------------------------
# Explore model data ----

# kickoffs_model %>%
#   group_by(return_type) %>%
#   tally() %>%
#   mutate(freq = n/ sum(n)) %>%
#   ggplot(., aes(x = return_type, y = n, label = paste(round(freq*100, 2), "%"))) + 
#   geom_col(fill = c("cornflowerblue", "tomato"), alpha = .75) + 
#   geom_label() + 
#   theme_minimal() + 
#   labs(x = "", y = "Count", title = "80% of Endzone Kickoffs Result in a Touchback")
# 
# numeric_cols <- 
#   kickoffs_model %>% 
#   select(where(is.numeric)) %>%
#   names
# 
# kickoffs_model %>% 
#   select(where(is.numeric)) %>%
#   select(-ends_with("id")) %>%
#   correlate() %>%
#   network_plot()
# 
# numeric_boxplots <-
#   function(col_name){
#     
#     # col_name <- "recieving_team_score_diff"
#       
#     kickoffs_model %>%
#       select(return_type, one_of({{col_name}})) %>%
#       rename(predictor = 2) %>%
#       ggplot(., aes(x = return_type, y = predictor, fill = return_type)) + 
#       geom_jitter(alpha = .3, color = 'gray') + 
#       geom_boxplot(alpha = .75, outlier.shape = NA) + 
#       coord_flip() + 
#       theme_minimal() + 
#       scale_fill_manual(values = c("cornflowerblue", "tomato")) + 
#       labs(x = "", y = "", title = paste("Return type vs", {{col_name}}))
#   }
# 
# map(numeric_cols, ~numeric_boxplots(.x))
# 
# factor_cols <- 
#   kickoffs_model %>% 
#   select(-return_type) %>%
#   select(where(is.factor)) %>%
#   names
# 
# factor_bar_plots <-
#   function(col_name){
#     
#     kickoffs_model %>%
#       select(return_type, one_of({{col_name}})) %>%
#       rename(predictor = 2) %>%
#       group_by(predictor, return_type) %>%
#       tally() %>%
#       mutate(freq = n / sum(n)) %>%
#       ggplot(., aes(x = reorder(predictor, -freq), y = freq, fill = return_type, label = paste0(paste0(round(freq*100, 2), "%"), "\n(n = ", n, ")"))) + 
#       geom_col(position = 'dodge', alpha = .75) + 
#       geom_text(position = position_dodge(width = .9), vjust = -0.5) + 
#       theme_minimal() + 
#       scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .10)) + 
#       scale_fill_manual(values = c("cornflowerblue", "tomato")) + 
#       labs(x = "", y = "Count", title = "80% of Endzone Kickoffs Result in a Touchback") + 
#       labs(x = "", y = "", title = paste("Return type vs", {{col_name}}))
#   }
# 
# map(factor_cols, ~factor_bar_plots(.x))
# 
# factor_mosaic_plots <-
#   function(col_name){
#     
#     kickoffs_model %>%
#       select(return_type, one_of({{col_name}})) %>%
#       rename(predictor = 2) %>%
#       group_by(predictor, return_type) %>%
#       tally() %>%
#       mutate(freq = n / sum(n)) %>%
#       ggplot(., aes(label = paste0(paste0(round(freq*100, 2), "%"), "\n(n = ", n, ")"))) +
#       geom_mosaic(aes(x = product(predictor, return_type), fill = predictor, weight = freq), na.rm = T, show.legend = F,  offset = 0.001) + 
#       theme_minimal() + 
#       labs(x = "", y = "Count", title = "80% of Endzone Kickoffs Result in a Touchback") + 
#       labs(x = "", y = "", title = paste("Return type vs", {{col_name}}))
#   }
# 
# map(factor_cols, ~factor_mosaic_plots(.x))

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

# folds <-
#   rsample::vfold_cv(training, v = 20, strata = return_type)

# define metrics
kickoff_metrics <- 
  metric_set(accuracy, sens, spec, roc_auc)

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

# juice(
#   prep(recipe)
#   ) %>% View

#------------------------------------------------
# model sepcs ----
glm_spec <- 
  logistic_reg(
    penalty = tune(),
    mixture = tune()) %>% 
  set_engine("glmnet")

# pls_spec <- 
#   pls(
#     predictor_prop = tune(), 
#     num_comp = tune()) %>%
#   set_engine("mixOmics") %>%
#   set_mode("classification")

tree_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

bag_tree_spec <- 
  bag_tree(
    tree_depth = tune(),
    min_n = tune(),
    class_cost = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

boost_tree_spec <-
  boost_tree(
    mtry = tune(),
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    stop_iter = tune()
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

rand_forest_spec <- 
  rand_forest(
    trees = 1000,
    min_n = tune(), 
    mtry = tune()
    ) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

mars_spec <- 
  mars(
    num_terms = tune(),
    prod_degree = tune(),
    prune_method = tune()
    ) %>% 
  set_engine("earth") %>% 
  set_mode("classification")

bag_mars_spec <- 
  bag_mars(
    num_terms = tune(),
    prod_degree = tune(),
    prune_method = tune()
  ) %>% 
  set_engine("earth") %>% 
  set_mode("classification")

mars_discrim_spec <- 
  discrim_flexible(
    prod_degree = tune()
    ) %>% 
  set_engine("earth")

mlp_spec <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    epochs = tune()
    )%>% 
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

lin_discrim_spec <- 
  discrim_linear(
    penalty = tune(),
    regularization_method = tune()) %>% 
  set_engine("MASS") %>% 
  set_mode("classification")

knn_spec <- 
  nearest_neighbor(
    neighbors = tune(),
    weight_func = tune(),
    dist_power = tune()
  ) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

discrim_reg_spec <- 
  discrim_regularized(
    frac_common_cov = tune(),
    frac_identity = tune()) %>% 
  set_engine("klaR") %>% 
  set_mode("classification")

#------------------------------------------------
# workflowsets ----

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
    models = list(
      bag_mars = bag_mars_spec,
      # bag_tree = bag_tree_spec, bad performance
      # naive_bayes = bayes_spec, bad performance
      boost_tree = boost_tree_spec,
      discrim_reg = discrim_reg_spec,
      glm = glm_spec,
      # knn = knn_spec, #issue - model doesn't work
      mars_discrim = mars_discrim_spec,
      mars = mars_spec,
      nnet = mlp_spec,
      random_forest = rand_forest_spec, #issue - model doesn't work
      svm = svm_spec,
      decision_tree = tree_spec
      )
  ) %>%
  option_add(param_info = update_mtry(., "recipe_random_forest"), id = "recipe_random_forest") %>%
  option_add(param_info = update_mtry(., "recipe_boost_tree"), id = "recipe_boost_tree")
  
grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    allow_par = TRUE,
    parallel_over = "everything",
    verbose = TRUE
  )

race_ctrl <-
  control_race(
    save_pred = TRUE,
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything"
  )

cl <- 
  makeCluster(10)

doParallel::registerDoParallel(cl)

tictoc::tic()
results <- 
  all_workflows %>%
  workflow_map(
    seed = 1503,
    control = grid_ctrl,
    fn = "tune_grid",
    # control = race_ctrl,
    # fn = "tune_race_anova",
    resamples = folds,
    grid = 10,
    verbose = T,
    metrics = kickoff_metrics
  )
tictoc::toc()

stopCluster(cl)

# # check notes
# results %>%
#   unnest(result) %>%
#   unnest(.notes) %>%
#   select(wflow_id, .notes) %>% View
#
# # results quick check
# rank_results(results, "accuracy", select_best = "TRUE")
#           
# #------------------------------------------------
# # save experiment ----
# 
# workflowset_experiment <-
#   tibble(workflows = list(results),
#          splits = list(splits))
# 
# save_experiment(experiment = workflowset_experiment,
#                 workflow_name = "v0.0.3")
# 
# stopCluster(cl)
