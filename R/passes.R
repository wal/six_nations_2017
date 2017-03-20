source('common.R')

passes_by_player <- tournament_data %>% 
  group_by(team, position_type, player_name) %>% 
  summarise(passes = sum(passes), 
            bad_passes = sum(bad_passes),
            matches = n(),
            game_minutes = sum(minutes_played_total)
  ) %>%
  arrange(desc(passes))
