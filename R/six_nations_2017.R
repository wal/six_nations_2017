library(tidyverse)
library(gridExtra)

tournament_data = read.csv(file = 'data/tournament.csv')

# Variables
metric_names = sort(names(tournament_data))
metric_names[grep('turn', metric_names)]

# Tackles

tackles_by_player <- tournament_data %>% 
  group_by(team, player_name) %>% 
  summarise(tackles = sum(tackles), 
            missed_tackles = sum(missed_tackles), 
            tackle_success = mean(tackle_success), 
            matches = n(),
            game_minutes = sum(minutes_played_total),
            tackles_per_game = tackles / matches
  ) %>%
  arrange(desc(tackle_success)) %>%
  filter(tackles > 50)
nrow(tackles_by_player)
head(tackles_by_player, n = 20, wt = tackle_success)


# Carries

carries_by_player <- tournament_data %>% 
  group_by(team, player_name) %>% 
  summarise(carries_metres = sum(carries_metres), 
            carries_crossed_gain_line = sum(carries_crossed_gain_line), 
            carries_not_made_gain_line = sum(carries_not_made_gain_line), 
            carries_support = sum(carries_support), 
            carries = carries_crossed_gain_line + carries_not_made_gain_line + carries_support,
            matches = n(),
            game_minutes = sum(minutes_played_total),
            gain_no_gain = carries_crossed_gain_line / carries_not_made_gain_line,
            meters_per_carry = carries_metres / carries
  ) %>%
  arrange(desc(carries)) %>%
  filter(carries > 50)
nrow(carries_by_player)
head(carries_by_player, n = 20, wt = carries) %>% select(player_name, carries, meters_per_carry)



# Offload

offloads_by_player <- tournament_data %>% 
  group_by(team, player_name) %>% 
  summarise(offload = sum(offload), 
            matches = n(),
            game_minutes = sum(minutes_played_total)
  ) %>%
  arrange(desc(offload))
nrow(offloads_by_player)
head(offloads_by_player, n = 20, wt = offload)

# Passes

passes_by_player <- tournament_data %>% 
  group_by(team, player_name) %>% 
  summarise(passes = sum(passes), 
            bad_passes = sum(bad_passes),
            matches = n(),
            game_minutes = sum(minutes_played_total)
  ) %>%
  arrange(desc(passes))
nrow(passes_by_player)
head(passes_by_player, n = 20, wt = passes)

# Turnovers

turnovers_by_player <- tournament_data %>%
  group_by(team, player_name) %>%
  summarise(
    turnover_won = sum(turnover_won),
    turnovers_conceded = sum(turnovers_conceded)
  ) %>% arrange(desc(turnovers_conceded))
nrow(turnovers_by_player)
head(turnovers_by_player, n = 20, wt = turnovers_conceded)



# Total Lineouts

ggplot(data = tournament_data) + 
  geom_bar(mapping = aes(x = team, y = lineouts_lost, fill = opposition), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))


ggplot(data = tournament_data) +
  geom_bar(mapping = aes(y = offload, x = team), stat = 'identity')

