library(tidyverse)
library(gridExtra)

tournament_data = read.csv(file = 'data/tournament.csv')

# Variables
sort(names(tournament_data))

# Tackles

top_20_tackles_by_player <- tournament_data %>% 
  group_by(player_name) %>% 
  summarise(tackles = sum(tackles)) %>%
  top_n(20, wt = tackles)

top_20_tackles_by_player %>% order(tackles)

# Turnovers

turnover_won_plot <- ggplot(data = tournament_data) +
  geom_bar(mapping = aes(y = turnover_won, x = team), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))

turnover_conceeded_plot <- ggplot(data = tournament_data) +
  geom_bar(mapping = aes(y = turnovers_conceded , x = team), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))

grid.arrange(turnover_won_plot, turnover_conceeded_plot)


# Total Lineouts

ggplot(data = tournament_data) + 
  geom_bar(mapping = aes(x = team, y = lineouts_lost, fill = opposition), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))
  

offloads_plot <- ggplot(data = tournament_data) +
  geom_bar(mapping = aes(y = offload, x = team), stat = 'identity')

grid.arrange(passes_plot, offloads_plot)

res <- cor(tournament_data)

ireland_data <- tournament_data %>% filter(team == 'Ireland')

ggplot(data = ireland_data) +
  geom_boxplot(mapping = aes(x = player_name, y = carries_metres)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(~ opposition)

