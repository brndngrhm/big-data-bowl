#
# ABOUT: This script analyzes & compares various medical elective LOS models. Should be run as `Source as Local Job`. change working directory to root in dialogue box, copy results to global environment.
#
# -------------------------------------------------------
# load packages and source functions ----

library(here)
library(dplyr)
library(reactable)
library(tidymodels)
library(themis)
library(workflowsets)
library(purrr)
library(furrr)
library(doParallel)
library(stringr)
library(rocqi)
library(htmlwidgets)
library(htmltools)
library(stringr)

source(here("R", "util.R"))

# -------------------------------------------------------
# load models ----

# load model workflowset
class_workflows <- 
  readRDS(here::here("R", "experiments", "2021_12_26_1701_v0.0.2.rds"))

splits <- 
  class_workflows %>% 
  ungroup() %>%
  dplyr::select(splits) %>%
  .[[1]] %>%
  .[[1]]

workflow_models <- 
  # tune_results %>% filter(!wflow_id %in% c("basic_recipe_mars", "no_smote_rec_mars"))
  class_workflows %>%
  ungroup() %>%
  dplyr::select(workflows) %>%
  .[[1]] %>%
  .[[1]]

workflow_models %>%
  unnest(info) %>%
  select(workflow)%>%
  .[[1]] %>%
  .[[1]]

# -------------------------------------------------------
# rank models ----

rank_results(workflow_models, "accuracy", select_best = "TRUE")
rank_results(workflow_models, "sens", select_best = "TRUE")
rank_results(workflow_models, "spec", select_best = "TRUE")
rank_results(workflow_models, "roc_auc", select_best = "TRUE")

# rank table
cutoff <- 15

rank_table <- 
  get_class_model_rank(workflow_models, rank_cutoff = cutoff, type = "wide") %>%
  reactable(., sortable = T, searchable = T, filterable = T)

title <-
  glue::glue("Top {cutoff} Regression Models per Metric ({format(nrow(collect_metrics(workflow_models, summarize = FALSE))/4, big.mark = ',')} models evaluated)")

htmlwidgets::prependContent(rank_table, 
                            h2(class = "title", title))

# -------------------------------------------------------
# individual metric plots ----

spec_plot <- 
  autoplot(workflow_models, metric = "spec", select_best = TRUE) + labs(x = "Rank", title = "Models ranked by Specificity")
sens_plot <- 
  autoplot(workflow_models, metric = "sens", select_best = TRUE) + labs(x = "Rank", title = "Models ranked by Sensitivity")
acc_plot <- 
  autoplot(workflow_models, metric = "accuracy", select_best = TRUE) + labs(x = "Rank", title = "Models ranked by Accuracy")
roc_auc_plot <-
  autoplot(workflow_models, metric = "roc_auc", select_best = TRUE) + labs(x = "Rank", title = "Models ranked by ROC_AUC")

ggpubr::ggarrange(acc_plot, sens_plot, spec_plot, roc_auc_plot, ncol = 2, nrow = 2)

# -------------------------------------------------------
# ROC AUC curves ----

workflow_models %>%
  unnest(result) %>%
  unnest(.predictions) %>%
  inner_join(., rank_results(workflow_models, "roc_auc", select_best = "TRUE"), by = c("wflow_id", ".config")) %>%
  dplyr::select(wflow_id, return_type, starts_with(".pred")) %>%
  distinct() %>%
  group_by(wflow_id) %>%
  roc_curve(., return_type, `.pred_Endzone Return`) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = wflow_id)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  facet_wrap(.~wflow_id) +
  theme(legend.position = "none") +
  labs(title = "ROC AUC Curves")

# -------------------------------------------------------
# combined metric plots ----

get_class_model_rank(workflow_models, rank_cutoff = cutoff, type = "long")  %>% 
  ggplot(., aes(x = rank, y = mean, color = factor(wflow_id))) +
  geom_point(size = 5, aes(fill = factor(wflow_id), alpha = factor(pca_type))) +
  facet_wrap(. ~ .metric) +
  theme(legend.position = "right") + 
  scale_color_chop() +
  scale_alpha_manual(values = c("none" = .9, "pca" = .3)) + 
  labs(x = "rank", y = "avg value") + 
  scale_x_continuous(breaks = seq(1, cutoff, 1))

# workflow_models %>%
#   extract_workflow_set_result("base_rec_cubist") %>% 
#   # select_best(metric = "rmse") %>%
#   workflowsets::collect_predictions(., summarize = TRUE) 

# -------------------------------------------------------
# feature importance ----
best_results <- 
  workflow_models %>% 
  extract_workflow_set_result("recipe_boost_tree") %>% 
  select_best(metric = "roc_auc")

library(vip)
workflow_models %>% 
  extract_workflow("recipe_boost_tree") %>% 
  finalize_workflow(best_results) %>%
  fit(data = training) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 15)

xgb.importance(model = bst)

# -------------------------------------------------------
# model finalize ----

best_results <- 
  workflow_models %>% 
  extract_workflow_set_result("rec_glm") %>% 
  select_best(metric = "roc_auc")

test_results <- 
  workflow_models %>% 
  extract_workflow("rec_glm") %>% 
  finalize_workflow(best_results) %>% 
  last_fit(split = splits)

collect_metrics(test_results)

bind_cols(test_results %>% 
            collect_predictions() %>% dplyr::select(.pred_class),
          test) %>%
  dplyr::select(target, .pred_class) %>%
  group_by(target, .pred_class) %>%
  tally() %>%
  mutate(freq = n / sum(n)) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = .pred_class, values_from = freq)


bind_cols(test_results %>% 
            collect_predictions() %>% dplyr::select(.pred_class),
          test) %>%
  group_by(target, .pred_class) %>%
  tally() %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = target, y = .pred_class, size = n)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = 0.5) + 
  # coord_obs_pred() + 
  labs(x = "observed", y = "predicted")

preds <-
  workflowsets::collect_predictions(workflow_models, summarize = TRUE) 

preds %>%
  filter(wflow_id == "rec_glm") %>%
  conf_mat(target, .pred_class) %>%
  autoplot(., type = "heatmap")

