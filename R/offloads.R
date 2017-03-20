source('common.R')

offloads_by_player <- tournament_data %>% 
  group_by(team, position_type, player_name) %>% 
  summarise(offload = sum(offload), 
            matches = n(),
            game_minutes = sum(minutes_played_total)
  ) %>%
  arrange(desc(offload))

