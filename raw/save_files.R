library(tidyverse)

info <- nflfastR::load_pbp(2018) %>% 
  dplyr::filter(complete_pass == 1 | incomplete_pass == 1) %>%
  dplyr::rename(nflfastr_id = game_id, game_id = old_game_id) %>%
  dplyr::select(posteam, yardline_100, defteam, home_team, away_team, down, ydstogo, week, nflfastr_id, game_id, qtr, play_id, epa, yards_gained, air_yards, home_wp, epa, desc, complete_pass) %>%
  dplyr::mutate(game_id = as.numeric(game_id))

saveRDS(info, "data/nflfastR_2018.rds")
