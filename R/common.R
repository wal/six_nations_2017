library(tidyverse)

tournament_data = read.csv(file = 'data/tournament.csv')

# Add position type and position number
tournament_data <- tournament_data %>% separate(position, c('position_type', 'position_numnber'))
