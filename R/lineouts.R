source('common.R')


metric_names = sort(names(tournament_data))
metric_names[grep('lineout', metric_names)]

lineouts_by_team <- tournament_data %>% 
  group_by(team) %>%
  summarise(
    total_lineouts = sum(total_lineouts),
    lineouts_won = sum(lineouts_won), 
    lineouts_lost = sum(lineouts_lost),
    win_percentage = round(lineouts_won/total_lineouts * 100, 1)
  ) %>% arrange(desc(win_percentage))

ireland_lineouts_lost_by_opposition <- tournament_data %>% 
  filter(team == 'Ireland') %>%
  group_by(team, opposition) %>%
  summarise(
    total_lineouts = sum(total_lineouts),
    lineouts_won = sum(lineouts_won), 
    lineouts_lost = sum(lineouts_lost),
    win_percentage = round(lineouts_won/total_lineouts * 100, 1)
  ) %>% arrange(desc(lineouts_lost))

ireland_lineout_success_by_player <- tournament_data %>% 
  filter(team == 'Ireland', lineout_success > 0) %>%
  group_by(team, opposition, player_name) %>%
  summarise(
    total_lineouts = sum(total_lineouts),
    lineout_success = round(mean(lineout_success) * 100, 1),
    lineout_throw_lost_outright = sum(lineout_throw_lost_outright),
    lineout_throw_lost_handling_error = sum(lineout_throw_lost_handling_error),
    lineout_throw_lost_not_straight = sum(lineout_throw_lost_not_straight)
  ) %>% filter(lineout_success < 100)

ireland_lineouts_stolen <- tournament_data %>%
  filter(team == 'Ireland', lineout_won_steal > 0) %>%
  group_by(team, opposition, player_name) %>%
  summarise(lineouts_stolen = sum(lineout_won_steal))


lineouts_stolen <- tournament_data %>%
  group_by(team, player_name) %>%
  summarise(lineout_won_steal = sum(lineout_won_steal)) %>% 
  arrange(desc(lineout_won_steal)) %>% 
  filter(lineout_won_steal > 0)

lineouts_stolen_against <- tournament_data %>%
  group_by(opposition) %>%
  summarise(lineout_won_steal = sum(lineout_won_steal)) %>% 
  arrange(desc(lineout_won_steal)) %>% 
  filter(lineout_won_steal > 0)

lineouts_won <- tournament_data %>%
  group_by(player_name) %>%
  summarise(
    lineout_throw_won_tap = sum(lineout_throw_won_tap),
    lineout_throw_won_penalty = sum(lineout_throw_won_penalty),
    lineout_throw_lost_penalty = sum(lineout_throw_lost_penalty),
    something = (lineout_throw_won_tap + lineout_throw_won_penalty + lineout_throw_lost_penalty)
  ) %>% filter(something > 0)