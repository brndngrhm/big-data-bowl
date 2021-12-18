
library(dplyr)
library(purrr)
library(here)
library(readr)
library(feather)

csv_to_feather <-
  function(file_name){
    read_csv(here("data", "raw", {{file_name}})) %>%
      write_feather(., path = here("data", "raw", paste0(stringr::str_remove({{file_name}}, pattern = ".csv"), "_raw.feather")))
  }

# read in .csv files, save as each as a .feather file
list.files(path = here("data", "raw"), pattern = ".csv") %>%
  map(., ~csv_to_feather(.x))
