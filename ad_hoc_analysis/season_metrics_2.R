library(dplyr)

year_round <- afl_tables_match_results %>%
        group_by(Season, Round) %>%
        summarise(ave_score = mean(Team1Points),
                  count = n()/2) %>%
        filter(ave_score <= 79.4 & Round == "R1")


year_team <- historic_ladders %>%
        ungroup() %>%
        filter(Played <= 12) %>%
        group_by(Season, Team1) %>%
        summarise(ave_score = mean(Team1Points),
                  sum_goals = sum(Team1Goals),
                  sum_behinds = sum(Team1Behinds),
                  agg_acc = sum_goals/(sum_goals + sum_behinds),
                  count = n())

summary_seasons <- afl_tables_match_results %>%
        ungroup() %>%
        group_by(Season) %>%
        summarise(mean_score = mean(Team1Points),
                  sd_score = sd(Team1Points),
                  ratio = sd_score/mean_score)


test <- historic_ladders %>%
        filter(Played == 10) %>%
        filter(AverageAgainst < 94) %>%
        group_by(Season) %>%
        summarise(count = n())