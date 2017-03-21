source('R/common.R')

metric_names = sort(names(tournament_data))
metric_names[grep('su', metric_names)]

tackles_by_player <- tournament_data %>% 
  group_by(team) %>% 
  summarise(tackles = sum(tackles), 
            missed_tackles = sum(missed_tackles), 
            tackle_win_pct = (1 - (missed_tackles/tackles)) * 100) %>%
        arrange(desc(tackle_win_pct))


# , 
game_minutes = sum(minutes_played_total)