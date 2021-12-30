
library(tidyverse)
library(feather)
library(nflfastR)

# get and save play-by-play data
nflfastR_pbp_2010_2020 <-
  nflfastR::load_pbp(2010:2020)

write_feather(x = nflfastR_pbp_2010_2020,
              path = here("data", "raw", "nflfastR_pbp_2010_2020.feather"))