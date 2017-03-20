source('common.R')

tackles_by_player <- tournament_data %>% 
  group_by(team, position_type, player_name) %>% 
  summarise(tackles = sum(tackles), 
            missed_tackles = sum(missed_tackles), 
            tackle_success = mean(tackle_success), 
            matches = n(),
            game_minutes = sum(minutes_played_total),
            tackles_per_game = tackles / matches) 


