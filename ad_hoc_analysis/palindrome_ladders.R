top_bottom <- historic_ladders %>%
        select(Season, Team1, Roundadj, Round, LadderRank, SeasonTeams, RunningWins, RunningDraws, RunningLosses) %>%
        #filter(Season == 2019 & Roundadj == 11) %>%
        arrange(LadderRank)

bottom_top <- historic_ladders %>%
        select(Season, Team1, Roundadj, Round, LadderRankBottom, RunningWins, RunningDraws, RunningLosses) %>%
        #filter(Season == 2019 & Roundadj == 11) %>%
        rename(LadderRank = LadderRankBottom) %>%
        arrange(LadderRank)

joined <- top_bottom %>%
        left_join(bottom_top, by = c("Season", "Roundadj", "LadderRank")) %>%
        filter(RunningWins.x == RunningLosses.y) %>%
        filter(RunningLosses.x == RunningWins.y) %>% 
        filter(RunningDraws.x == RunningDraws.y) %>%
        ungroup() %>%
        group_by(Season, Roundadj) %>%
        summarise(count = n(),
                  count_teams = max(SeasonTeams),
                  count_draws = sum(RunningDraws.x)) %>%
        filter(count == count_teams)
        

