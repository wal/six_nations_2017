source('common.R')

turnovers_by_player <- tournament_data %>%
  group_by(team, position, player_name) %>%
  summarise(
    turnover_won = sum(turnover_won),
    turnovers_conceded = sum(turnovers_conceded)
  ) %>% arrange(desc(turnovers_conceded))

