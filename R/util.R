
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

load_data <- 
  function(file_name){
    feather::read_feather(here("data", {{file_name}}))
  }

load_tracking_data <- 
  function(file_name){
    feather::read_feather(here("data", "tracking", {{file_name}}))
  }
