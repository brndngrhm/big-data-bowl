
round_numerics <- 
  function(data){
    data %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
  }

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
