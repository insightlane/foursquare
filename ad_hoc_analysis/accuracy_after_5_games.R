
historic_ladders %>%
        filter(Played == 5) %>%
        mutate(accuracy = RunningGoals/(RunningGoals + RunningBehinds)) %>%
        mutate(rank = rank(-accuracy)) %>%
        select(Season, Roundadj, Team1, RunningGoals, RunningBehinds, accuracy, rank) %>%
        arrange(accuracy) %>%
        filter(Season >= 1990)