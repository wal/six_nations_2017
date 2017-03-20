source('common.R')


effect_of_replacements <- tournament_data %>% filter(position_type == 'Replacement') %>% group_by(team, position_type) %>%
  summarise(tries = sum(tries),
            carries_metres = sum(carries_metres), 
            carries_crossed_gain_line = sum(carries_crossed_gain_line), 
            carries_not_made_gain_line = sum(carries_not_made_gain_line), 
            carries_support = sum(carries_support), 
            carries = carries_crossed_gain_line + carries_not_made_gain_line + carries_support,
            handling_error = sum(handling_error), 
            clean_breaks = sum(clean_breaks),
            offload = sum(offload),
            game_minutes = sum(minutes_played_total)
  )

carries_by_team_squad <- tournament_data %>% group_by(team, position_type) %>%
  summarise(
    carries_crossed_gain_line = sum(carries_crossed_gain_line), 
    carries_not_made_gain_line = sum(carries_not_made_gain_line), 
    carries_support = sum(carries_support), 
    carries = carries_crossed_gain_line + carries_not_made_gain_line + carries_support)


carries_by_player <- tournament_data %>% 
  group_by(team, position_type, player_name) %>% 
  summarise(carries_metres = sum(carries_metres), 
            carries_crossed_gain_line = sum(carries_crossed_gain_line), 
            carries_not_made_gain_line = sum(carries_not_made_gain_line), 
            carries_support = sum(carries_support), 
            carries = carries_crossed_gain_line + carries_not_made_gain_line + carries_support,
            matches = n(),
            game_minutes = sum(minutes_played_total),
            gain_no_gain = carries_crossed_gain_line / carries_not_made_gain_line,
            meters_per_carry = carries_metres / carries)