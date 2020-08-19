library(dplyr)


year <- afl_tables_match_results %>%
        ungroup() %>%
        group_by(Season) %>%
        summarise(ave_score = mean(Team1Points),
                  med_score = median(Team1Points),
                  games = n()/2)


year_round <- afl_tables_match_results %>%
        ungroup() %>%
        group_by(Season, Round) %>%
        summarise(ave_score = mean(Team1Points),
                  med_score = median(Team1Points),
                  sd_score  = sd(Team1Points),
                  games = n()/2,
                  ave_win_Score = 


topbottomhalf <- historic_ladders %>%
        filter(!is.na(PreRankTeam2)) %>%
        mutate(Opp_Position = ifelse(PreRankTeam2 <= 9, "Top half", "Bottom half")) %>%
        filter(Season >= 2013) %>%
        filter(Team1 == "Geelong") %>%
        group_by(Opp_Position, Result) %>%
        summarise(count = n())