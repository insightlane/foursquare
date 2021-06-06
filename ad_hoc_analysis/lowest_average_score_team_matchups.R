library(dplyr)

test <- afl_tables_match_results %>%
  mutate(team_alpha = ifelse(Team1 < Team2, Team1, Team2),
         team_beta = ifelse(Team1 > Team2, Team1, Team2)) %>%
  mutate(Team1Points = ifelse(Season == 2020, 1.25 * Team1Points, Team1Points)) %>%
  filter(Season >= 2017) %>%
  group_by(team_alpha, team_beta) %>%
  summarise(ave_score = mean(Team1Points) * 2,
            count_games = n()/2)