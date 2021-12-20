
library(tidyverse)
library(feather)
library(nflfastR)

games_2018 <-
  nflfastR::load_pbp(2018)

games_2019 <-
  nflfastR::load_pbp(2019)

games_2020 <-
  nflfastR::load_pbp(2020)

nflfastR_data <- 
  rbind(games_2018, games_2019, games_2020)

write_feather(x = nflfastR_data, path = here("data", "raw", "nflfastR_data.feather"))

