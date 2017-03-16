library(tidyverse)
library(gridExtra)

tournament_data = read.csv(file = 'data/tournament.csv')

# Add position type and position number
tournament_data <- tournament_data %>% separate(position, c('position_type', 'position_numnber'))

# Variables
metric_names = sort(names(tournament_data))


# Tackles
tackles_by_player <- tournament_data %>% 
  group_by(team, position_type, player_name) %>% 
  summarise(tackles = sum(tackles), 
            missed_tackles = sum(missed_tackles), 
            tackle_success = mean(tackle_success), 
            matches = n(),
            game_minutes = sum(minutes_played_total),
            tackles_per_game = tackles / matches
  ) 


# Carries

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
            meters_per_carry = carries_metres / carries
  )

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




# Offload

offloads_by_player <- tournament_data %>% 
  group_by(team, position_type, player_name) %>% 
  summarise(offload = sum(offload), 
            matches = n(),
            game_minutes = sum(minutes_played_total)
  ) %>%
  arrange(desc(offload))

# Passes

passes_by_player <- tournament_data %>% 
  group_by(team, position_type, player_name) %>% 
  summarise(passes = sum(passes), 
            bad_passes = sum(bad_passes),
            matches = n(),
            game_minutes = sum(minutes_played_total)
  ) %>%
  arrange(desc(passes))

# Turnovers

turnovers_by_player <- tournament_data %>%
  group_by(team, position, player_name) %>%
  summarise(
    turnover_won = sum(turnover_won),
    turnovers_conceded = sum(turnovers_conceded)
  ) %>% arrange(desc(turnovers_conceded))


# Total Lineouts

ggplot(data = tournament_data) + 
  geom_bar(mapping = aes(x = team, y = lineouts_lost, fill = opposition), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))


ggplot(data = tournament_data) +
  geom_bar(mapping = aes(y = offload, x = team), stat = 'identity')

