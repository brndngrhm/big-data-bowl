library(reactable)
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))

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
                           showSortIcon = T, striped = T, compact = T, defaultExpanded = T)
  }

load_clean_data <- 
  function(file_name){
    feather::read_feather(here("data", "clean", {{file_name}}))
  }

load_raw_data <- 
  function(file_name){
    feather::read_feather(here("data", "raw", {{file_name}}))
  }

load_anaylsis_data <- 
  function(file_name, type){
    if(!type %in% c("raw", "clean", "analysis")) stop('type must be one of "raw", "clean", "analysis"')
    
    feather::read_feather(here("data", {{type}}, {{file_name}}))
  }

load_data <- 
  function(type, file_name){
    if(!type %in% c("raw", "clean", "analysis")) stop('type must be one of "raw", "clean", "analysis"')
    feather::read_feather(here("data", {{type}}, {{file_name}}))
  }

add_big_labels <- 
  function(text_size = 14){
    text = element_text(size = text_size)
  }

save_experiment <-
  function(experiment, workflow_name){
    
    date <- 
      format(Sys.time(), "%Y_%m_%d_%H%M")
    
    path <- 
      here("R", "experiments", glue::glue("{date}_{workflow_name}.rds"))
    
    saveRDS(experiment, path)
    
  }

get_class_model_rank <-
  function(workflow_models, rank_cutoff, type){
    
    if(!type %in% c("long", 'wide')) stop("type must be one of 'long', 'wide'")
    
    df <- 
      if(type == "long"){
        
        accuracy_rank <- 
          rank_results(workflow_models, rank_metric = "accuracy", select_best = TRUE) %>%
          filter(.metric == "accuracy",
                 rank <= rank_cutoff) %>%
          dplyr::select(wflow_id, .config, .metric, mean, std_err, model, rank)
        
        roc_auc_rank <- rank_results(workflow_models, rank_metric = "roc_auc", select_best = TRUE) %>%
          filter(.metric == "roc_auc",
                 rank <= rank_cutoff) %>%
          dplyr::select(wflow_id, .config, .metric, mean, std_err, model, rank)
        
        mn_log_loss_rank <- rank_results(workflow_models, rank_metric = "mn_log_loss", select_best = TRUE) %>%
          filter(.metric == "mn_log_loss",
                 rank <= rank_cutoff) %>%
          dplyr::select(wflow_id, .config, .metric, mean, std_err, model, rank)
        
        # sens_rank <- rank_results(workflow_models, rank_metric = "sens", select_best = TRUE) %>%
        #   filter(.metric == "sens",
        #          rank <= rank_cutoff) %>%
        #   dplyr::select(wflow_id, .config, .metric, mean, std_err, model, rank)
        
        accuracy_rank %>% 
          bind_rows(., roc_auc_rank) %>% 
          bind_rows(., mn_log_loss_rank)
          # bind_rows(., sens_rank)
        
      } else {
        
        accuracy_rank <- 
          rank_results(workflow_models, rank_metric = "accuracy", select_best = FALSE) %>%
          dplyr::select(wflow_id, .config, .metric, mean, std_err, model, accuracy_rank = rank)
        
        roc_auc_rank <-
          rank_results(workflow_models, rank_metric = "roc_auc", select_best = FALSE) %>%
          dplyr::select(wflow_id, .config, .metric, mean, std_err, model, roc_auc_rank = rank)
        
        mn_log_loss_rank <-
          rank_results(workflow_models, rank_metric = "mn_log_loss", select_best = FALSE) %>%
          dplyr::select(wflow_id, .config, .metric, mean, std_err, model, mn_log_loss_rank = rank)
        
        sens_rank <-
          rank_results(workflow_models, rank_metric = "sens", select_best = FALSE) %>%
          dplyr::select(wflow_id, .config, .metric, mean, std_err, model, sens_rank = rank)
        
        accuracy_rank %>% 
          left_join(., roc_auc_rank, by = c("wflow_id", ".config", ".metric", "mean", "std_err", "model")) %>% 
          left_join(., mn_log_loss_rank, by = c("wflow_id", ".config", ".metric", "mean", "std_err", "model")) %>% 
          # left_join(., sens_rank, by = c("wflow_id", ".config", ".metric", "mean", "std_err", "model")) %>% 
          filter(accuracy_rank <= rank_cutoff | roc_auc_rank <= rank_cutoff | mn_log_loss_rank <= rank_cutoff)
      }
    
    df %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      mutate(recipe_type = paste(map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1]),
                                 map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[2])),
             model = paste(map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[3])),
             model2 = paste(map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[4])),
             pca_type = ifelse(model %in% c("umap", "pca"), model, "none")) %>%
      dplyr::select(model, model2, recipe_type, pca_type, .metric, mean, ends_with("rank"), wflow_id, .config) %>%
      mutate(model = ifelse(model %in% c("umap", "pca"), model2, model)) %>%
      dplyr::select(-model2)
  }
