
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
library(htmlwidgets)
library(htmltools)
library(stringr)
library(forcats)
library(ggrepel)
library(ggthemr)

source(here("R", "util.R"))

# -------------------------------------------------------
# load models ----

# load model workflowset
class_workflows <- 
  readRDS(here::here("R", "experiments", "2021_12_28_2208_v0.0.5.rds"))

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

# -------------------------------------------------------
# rank models ----

rank_results(workflow_models, "accuracy", select_best = "TRUE")
rank_results(workflow_models, "mn_log_loss", select_best = "TRUE")
rank_results(workflow_models, "roc_auc", select_best = "TRUE")

# rank table
cutoff <- 15

rank_table <- 
  get_class_model_rank(workflow_models, rank_cutoff = cutoff, type = "wide") %>%
  reactable(., sortable = T, searchable = T, filterable = T)

title <-
  glue::glue(
    "Top {cutoff} Regression Models per Metric ({format(nrow(collect_metrics(workflow_models, summarize = FALSE))/4, big.mark = ',')} models evaluated)")

htmlwidgets::prependContent(rank_table, 
                            h2(class = "title", title))


get_class_model_rank(workflow_models, rank_cutoff = 1, type = "wide") %>%
  reactable(., sortable = T, searchable = T, filterable = T)

get_class_model_rank(workflow_models, rank_cutoff = 1, type = "wide") %>%
  select(model, ends_with("rank"))

# -------------------------------------------------------
# individual metric plots ----

colors <- 
  c("#F94144", "#F3722C", "#F8961E", "#F9C74F", "#90BE6D", "#43AA8B", "#577590",
    "#272932", "#002A22", "#9E768F", "#9E0059", "#585191")

mn_log_loss_plot <- 
  autoplot(workflow_models, metric = "mn_log_loss", select_best = TRUE) + 
  scale_color_manual(values = colors) + 
  labs(x = "Rank", title = "Models ranked by mn_log_loss") + 
  geom_text_repel(aes(label = model), nudge_y = .005, show.legend = FALSE) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(text = add_big_labels(12))

acc_plot <- 
  autoplot(workflow_models, metric = "accuracy", select_best = TRUE) +
  scale_color_manual(values = colors) + 
  labs(x = "Rank", title = "Models ranked by Accuracy") + 
  geom_text_repel(aes(label = model), nudge_y = -.005, show.legend = FALSE) +
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(text = add_big_labels(12))

roc_auc_plot <-
  autoplot(workflow_models, metric = "roc_auc", select_best = TRUE) + 
  scale_color_manual(values = colors) + 
  labs(x = "Rank", title = "Models ranked by ROC_AUC") + 
  geom_text_repel(aes(label = model), nudge_y = -.005, show.legend = FALSE) +
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(text = add_big_labels(12))

arranged_plot <-
  ggpubr::ggarrange(acc_plot, mn_log_loss_plot, roc_auc_plot, ncol = 2, nrow = 2)


ggsave(arranged_plot, filename = here::here("img", "model_metrics_comparison.svg"))

# -------------------------------------------------------
# ROC AUC curves ----

roc_auc_vals <- 
  rank_results(workflow_models, "roc_auc", select_best = "TRUE") %>%
  filter(.metric == "roc_auc") %>%
  select(wflow_id, model, mean_value = mean)

collect_predictions(workflow_models, select_best = TRUE, metric = "roc_auc") %>%
  group_by(model, wflow_id) %>%
  roc_curve(., return_type, `.pred_Endzone Return`) %>%
  inner_join(., roc_auc_vals, by = c("wflow_id", "model")) %>%
  mutate(label = paste(model, round(mean_value, 2)),
         model = as.factor(model)) %>%
  arrange(desc(mean_value)) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = fct_reorder(model, -mean_value))) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  facet_wrap(. ~ fct_reorder(label, -mean_value)) +
  theme_minimal() + 
  theme(legend.position = "none",
        text = add_big_labels(text_size = 14)) +
  labs(title = "ROC AUC Curves for Candidate Models",
       x = "1-Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate")

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
